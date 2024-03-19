
library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
-- this will be part of some larger tcp state machine
-- state machine will determine segment size, tcp length.
-- data must be completely collected first in order to calculate checksum.
entity tcp_header is
    generic (

    );
    port (
        clk                     : in  std_logic;
        rst                     : in  std_logic;


        fix_data_checksum       : in std_logic_vector (31 downto 0);
        o_tcp_rdy               : out std_logic;

        -- tcp header output checksum output =>
        o_tcp_header            : out std_logic_vector(63 downto 0);    -- 8 bytes
        o_data_valid            : out std_logic;
        o_data_keep             : out std_logic_vector(7 downto 0);

        hdr_data_valid          : in  std_logic;            -- this is kickedoff while fix4.2 checksum is being calculated
        --everything below =>  put into a record
        --pseudo header info
        src_ip_addr             : in std_logic_vector(31 downto 0);
        dest_ip_addr            : in std_logic_vector(31 downto 0);
        ip_protocol             : in std_logic_vector(7 downto 0);            -- := x"04";  --ipv4
        tcp_len                 : in std_logic_vector(15 dwonto 0);           -- up to 65,535bytes - 20bytes for tcp header.
        --tcp header
        src_port              : in std_logic_vector(15 downto 0);
        dest_port             : in std_logic_vector(15 downto 0);
        seq_num               : in std_logic_vector(31 downto 0);
        ack_num               : in std_logic_vector(31 downto 0);

        data_offset           : in std_logic_vector(3 downto 0);
        --reserved 3downto0 4bits..
        flags                 : in std_logic_vector(7 downto 0);
        window_size           : in std_logic_vector(15 downto 0);
        urg_ptr               : in std_logic_vector(15 downto 0)

    );
end entity tcp_header;

architecture rtl of tcp_header is
    signal sum0, sum1, sum2, sum3   : std_logic_vector(35 downto 0);
    signal sum01, sum23             : std_logic_vector(35 downto 0);
    signal susum0123                : std_logic_vector(35 downto 0);
    signal sum_fold                : std_logic_vector(19 downto 0);

    type state_type is (IDLE, CALC_CARRY, SEND_HDR0, SEND_HDR1, SEND_HDR2);
    signal ip_state : state_type := IDLE;    
begin

    --pre calc the pseudo header and actual header, leaving the datachecksum for
    process(clk) begin
        if risinge_edge(clk) then
            if (hdr_data_valid = '1') then
                psedu_ip_hdr00 <= src_ip_addr(31 downto 16);
                psedu_ip_hdr01 <= src_ip_addr(15 downto 0);

                psedu_ip_hdr10 <= dest_ip_addr(31 downto 16);
                psedu_ip_hdr11 <= dest_ip_addr(15 downto 0);

                psedu_ip_hdr20 <= x"00" & ip_protocol;
                psedu_ip_hdr21 <= tcp_len;

                tcp_hdr_00 <= src_port;
                tcp_hdr_01 <= dest_port;

                tcp_hdr_10 <= seq_num(31 downto 16);
                tcp_hdr_11 <= seq_num(15 downto 0);

                tcp_hdr_20 <= ack_num(31 downto 16);
                tcp_hdr_21 <= ack_num(15 downto 0);

                tcp_hdr_30 <= data_offset & x"0" & flags;
                tcp_hdr_31 <= window_size;

                -- tcp_hdr_40 <= dest_tcp_addr(31 downto 16);
                tcp_hdr_41 <= urg_ptr;
            end if;
        end if;
    end process;

    process(clk) begin
        if risinge_edge(clk) then
            --1clk  --16s into 32
            psum0 <= psedu_ip_hdr00 + psedu_ip_hdr01;
            psum1 <= psedu_ip_hdr10 + psedu_ip_hdr11;
            psum2 <= psedu_ip_hdr20 + psedu_ip_hdr21;

            sum0 <= tcp_hdr_00 + tcp_hdr_01;
            sum1 <= tcp_hdr_10 + tcp_hdr_11;
            sum2 <= tcp_hdr_20 + tcp_hdr_21;
            sum3 <= tcp_hdr_30 + tcp_hdr_31;
            -- sum4 <= tcp_hdr_40 + tcp_hdr_41;

            --2clk  -- 32s into 32
            psum01 <= psum0 + psum1;

            sum01 <= sum0 + sum1;
            sum23 <= sum2 + sum3;

            --3clk
            psum012 <= psum01 + psum2;
            
            sum0123 <= sum01 + sum23;

            --4clk
            sum5 <= sum0123 + x"0000" & tcp_hdr_41;

            --5clk
            sum6 <= sum5 + psum012;

            --6clk  -- take care of carry, 16b
            sum7 <= sum6(31 downto 16) + sum6(15 downto 0);

        end if;
    end process;

    --this step occurs when tcp datacheck calculation is complete/ready
    process(clk) begin
        if rising_edge(clk) then
            if (rst = '1') then
                tcp_state <= IDLE;
            else
                case (tcp_state) is
                    when IDLE =>
                        if (fix_rdy = '1') then
                            tcp_hdr_40 <= fix_data_checksum;  -- store here so same as what is used for calc
                            pre_sum <= sum7 + fix_data_checksum;
                            tcp_state <= CALC_CARRY;
                        end if;

                    when CALC_CARRY =>
                        tcp_hdr_checksum <= pre_sum(19 downto 16) + pre_sum(15 downto 0);

                        tcp_state <= SEND_HDR0;

                    when SEND_HDR2 =>
                        o_tcp_header <= tcp_hdr_checksum & ip_hdr_41 & x"0000_0000";
                    
                        tcp_state <= IDLE;

                    when others =>
                        tcp_state <= IDLE;

                end case;
            end if;
        end if;
    end process;

end rtl;


    process(all) begin
        case (tcp_state) is
            when IDLE =>
                if (fix_rdy = '1') then
                    -- HDR0: we can start outputting the header bc calc doesn't depend on this
                    o_tcp_header <=   tcp_hdr_00 & tcp_hdr_01 & tcp_hdr_10 & tcp_hdr_11;
                    o_keep <= x"1111_1111";
                    o_data_valid <= '1';
                    o_data_last <= '0';
                    o_tcp_rdy <= '0';
                else 
                    o_tcp_header <= (others '0');
                    o_keep <= x"0000_0000";
                    o_data_valid <= '0';
                    o_data_last <= '0';
                    o_tcp_rdy <= '0';
                end if;

            when CALC_CARRY =>
                -- HDR1:
                o_tcp_header <= tcp_hdr_20 & tcp_hdr_21 & ip_hdr_30 & ip_hdr_31;
                o_keep <= x"1111_1111";
                o_data_valid <= '1';
                o_data_last <= '0';
                o_tcp_rdy <= '0';

            when SEND_HDR2 =>
                o_tcp_header <= tcp_hdr_checksum & ip_hdr_41 & x"0000_0000";
            
                o_keep <= x"1111_1111";
                o_data_valid <= '1';
                o_data_last <= '1';
                o_tcp_rdy <= '1';

            when others =>
                o_tcp_header <= (others '0');
                o_keep <= x"0000_0000";
                o_data_valid <= '0';
                o_data_last <= '0';
                o_tcp_rdy <= '0';

        end case;
    end process;
end rtl;  