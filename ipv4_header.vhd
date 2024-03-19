
-- use 64bit, 8bytes, and the keep 7 downto 0, two rows.
-- NOT IMPLEMENTING OPTIONS!
library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ipv4_header is
    port (
        clk                 : in  std_logic;
        rst                 : in  std_logic;
        tcp_rdy             : in  std_logic;

        o_ipv4_header       : out std_logic_vector(63 downto 0);
        o_data_valid        : out std_logic;
        o_data_keep         : out std_logic_vector(7 downto 0);
        o_data_last         : out std_logic;
        --
        --everything below =>  put into a record
        version             : in std_logic_vector(3 downto 0);
        ihl                 : in std_logic_vector(3 downto 0);      -- 0 for simplification
        dscp                : in std_logic_vector(5 downto 0);
        ecn                 : in std_logic_vector(1 downto 0);
        --
        total_length        : in std_logic_vector(15 downto 0);     -- determined by algo/fix block
        --
        identification      : in std_logic_vector(15 downto 0);
        --
        flags               : in std_logic_vector(2 downto 0);
        fragment_offset     : in std_logic_vector(12 downto 0);

        time_to_live        : in std_logic_vector(7 downto 0);      -- external timer block

        ip_protocol         : in std_logic_vector(7 downto 0);      --constant

        src_ip_addr         : in std_logic_vector(31 downto 0);
        dest_ip_addr        : in std_logic_vector(31 downto 0)
    );

  end entity ipv4_header;

  architecture rtl of ipv4_header is

    -- in TX, we're not recieving =>  you have all the information.
    -- https://datatracker.ietf.org/doc/html/rfc1071
    -- a
    signal ip_hdr_00, ip_hdr_01 : std_logic_vector(15 downto 0);
    signal ip_hdr_10, ip_hdr_11 : std_logic_vector(15 downto 0);
    signal ip_hdr_20, ip_hdr_21 : std_logic_vector(7 downto 0);

    signal sum0, sum1, sum01, sum2 : std_logic_vector(47 downto 0);
    signal sum2 : std_logic_vector(31 downto 0);
    signal sum3 : std_logic_vector(19 downto 0);
    signal sum4 : std_logic_vector(15 downto 0);

    type state_type is (IDLE, CALC_CARRY, SEND_HDR0, SEND_HDR1, SEND_HDR2);
    signal ip_state : state_type := IDLE;

  begin
    -- i assume when algo/controller/engine determines it needs to
    -- use TCP/IP, or send whatever =>  all these data fields are available.
    -- it can be forwarded here while it is working on the FIX4.2 msg+checksum and TCP checksum/header.
    -- registering input =>  early on, will not add latency
    -- concatenate into 16b
    process(clk) begin
        if risinge_edge(clk) then
            ip_hdr_00 <= version & ihl & dscp & ecn;
            ip_hdr_01 <= total_length;

            ip_hdr_10 <= identification;
            ip_hdr_11 <= flags & fragment_offset;

            ip_hdr_20 <= x"00" & protocol;          -- leave out TTL
            -- ip_hdr_21 <= ip_checksum;

            ip_hdr_30 <= src_ip_addr(31 downto 16);
            ip_hdr_31 <= src_ip_addr(15 downto 0);

            ip_hdr_40 <= dest_ip_addr(31 downto 16);
            ip_hdr_41 <= dest_ip_addr(15 downto 0);

        end if;
    end process;

    process(clk) begin
        if risinge_edge(clk) then
            --1clk  --16s into 32
            sum0 <= ip_hdr_00 + ip_hdr_01;
            sum1 <= ip_hdr_10 + ip_hdr_11;
            -- sum2 is held off, and you checksum field set to 0 for calc
            sum3 <= ip_hdr_30 + ip_hdr_31;
            sum4 <= ip_hdr_40 + ip_hdr_41;

            --2clk  -- 32s into 32
            sum01 <= sum0 + sum1;
            sum34 <= sum3 + sum4;

            --3clk
            sum0134 <= sum01 + sum34;

            --4clk
            sum6 <= sum0134 + x"0000" & ip_hdr_20;

            --5clk  -- take care of carry, 16b
            sum7 <= sum6(31 downto 16) + sum6(15 downto 0);

        end if;
    end process;

    --this step occurs when TCP is done bc we need TTL to be most fresh
    process(clk) begin
        if rising_edge(clk) then
            if (rst = '1') then
                ip_state <= IDLE;
                o_ipv4_header <= (others => '0');
                o_keep <= (others => '0');
                o_data_valid <= '0';
                o_data_last <= '0';

            else
                case (ip_state) is
                    when IDLE =>
                        if (i_tcp_rdy = '1') then
                            ip_hdr_20 <= time_to_live;  -- store here so same as what is used for calc
                            pre_sum <= sum7 + time_to_live & x"00";
                            ip_state <= CALC_CARRY;

                            --HDR0
                            o_ipv4_header <=   ip_hdr_00 & ip_hdr_01 & ip_hdr_10 & ip_hdr_11;
                            o_keep <= x"1111_1111";
                            o_data_valid <= '1';
    
                        end if;

                    when CALC_CARRY =>
                        ip_hdr_checksum <= pre_sum(19 downto 16) + pre_sum(15 downto 0);

                        --HDR1
                        o_ipv4_header <= ip_hdr_20 & ip_protocol & ip_hdr_checksum & ip_hdr_30 & ip_hdr_31;
                        o_keep <= x"1111_1111";
                        o_data_valid <= '1';

                        ip_state <= SEND_HDR2;

                    when SEND_HDR2 =>
                        o_ipv4_header <= ip_hdr_40 & ip_hdr_41 & x"0000_0000";     -- do i need to send a keep signal? yes => eth_mac has data_in keep
                        o_keep <= x"1111_0000";
                        o_data_valid <= '1';
                        o_data_last <= '1';

                        ip_state <= IDLE;

                    when others =>
                        o_ipv4_header <= (others => '0');
                        o_keep <= x"0000_0000";
                        o_data_valid <= '0';
                        o_data_last <= '0';

                        ip_state <= IDLE;

                end case;
            end if;
        end if;
    end process;
  end rtl;