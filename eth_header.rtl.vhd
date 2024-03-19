
-- eth_header

library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity eth_header is
  generic (
    DST_MAC   : std_logic_vector(47 downto 0) := x"AABBCCDDEEFF";
    SRC_MAC   : std_logic_vector(47 downto 0) := x"AABBCCDDEEFF";
    ETH_TYPE  : std_logic_vector(15 downto 0) := x"0800"
  );
  port (
    clk                     : in  std_logic;
    rst                     : in  std_logic;

    -- serial_in               : in std_logic;
    data_in                 : in  std_logic_vector(63 downto 0);    -- 8 bytes
    data_in_valid           : in  std_logic;
    data_in_last            : in  std_logic;
    data_in_keep            : in  std_logic_vector(7 downto 0);

    -- serial_out              : out std_logic;
    data_out                : out std_logic_vector(63 downto 0);    -- 8 bytes
    data_out_valid          : out std_logic;
    data_out_last           : out std_logic;
    data_out_keep           : out std_logic_vector(7 downto 0)
  );
end entity eth_header;

architecture rtl of eth_header is

  -- reg/pipe input and keep value
  signal s0, s1 : std_logic_vector(63 downto 0) := (others => '0');
  signal k0, k1 : std_logic_vector(7 downto 0) := (others => '0');

  -- 5 states/sel, req 3bits
  type state_type is (RD_IDLE, RD_HDR_DATA, RD_DATA, RD_DATA_LAST0, RD_DATA_LAST1);
  signal rd_state : state_type := RD_IDLE;

begin


--------------------------------------------------------
-- INPUT REG/PIPE
--------------------------------------------------------
  process(clk) begin
    if rising_edge(clk) then

      if (data_in_valid = '1') then
        s0 <= data_in;
      end if;

      if (data_in_last = '1') then
        k0 <= data_in_keep;
      end if;

      s1 <= s0;
      k1 <= k0;

    end if;
  end process;


  --------------------------------------------------------
  -- DATA MUX
  --------------------------------------------------------
  process(all) begin
    case (rd_state) is
      when RD_IDLE =>
        if data_in_valid = '1' then
          data_out <= DST_MAC & SRC_MAC(47 downto 32);
          data_out_keep <= x"FF";
          data_out_valid <= '1';
        else 
          data_out <= (others => '0');
          data_out_keep <= (others => '0');
          data_out_valid <= '0';
        end if;
        
        data_out_last <= '0';

      when RD_HDR_DATA =>
        data_out <= SRC_MAC(31 downto 0) & ETH_TYPE & s0(63 downto 48);    
        data_out_keep <= x"FF";
        data_out_valid <= '1';
        data_out_last <= '0';

      when RD_DATA =>
        data_out <= s1(47 downto 0) & s0(63 downto 48);
        data_out_keep <= x"FF";
        data_out_valid <= '1';
        data_out_last <= '0';

      when RD_DATA_LAST0 =>
        
        data_out <= s1(47 downto 0) & s0(63 downto 48);
        data_out_valid <= '1';

        if (k0 = x"80" or k0 = x"C0") then -- means keep 1 or 2 byte 
          data_out_keep <= "11" & x"F" & k0(7 downto 6);
          data_out_last <= '1';
        else  
          data_out_keep <= x"FF";
          data_out_last <= '0';
        end if;

      when RD_DATA_LAST1 =>
        data_out <= s1(47 downto 0) & x"0000";
        data_out_keep <= k1(5 downto 0) & "00";
        data_out_valid <= '1';
        data_out_last <= '1';

      when others =>
        data_out <= (others => '0');
        data_out_keep <= (others => '0');
        data_out_valid <= '0';
        data_out_last <= '1';

    end case;
  end process;

  --------------------------------------------------------
  -- READ FSM or "output state machine"
  --------------------------------------------------------
  process(clk) begin
    if rising_edge(clk) then
      if(rst = '1') then
        rd_state <= RD_IDLE;
      else
        case (rd_state) is

          when RD_IDLE =>
            --hang around until..

            if data_in_valid = '1' then     -- valid kick off.
              rd_state <= RD_HDR_DATA;
            end if;          

          when RD_HDR_DATA =>
            -- because assumption is valid is assertd for entire packet, auto transition.
            rd_state <= RD_DATA;            
                       
          when RD_DATA =>

            if (data_in_last = '1') then 
              rd_state <= RD_DATA_LAST0;
            end if;

          when RD_DATA_LAST0 =>
            
            -- means keep 1 or 2 byte => 
            if (k0 = x"80" or k0 = x"C0") then
              rd_state <= RD_IDLE; -- DONE
            else  
              rd_state <= RD_DATA_LAST1;
            end if;

          when RD_DATA_LAST1 =>

            rd_state <= RD_IDLE; -- DONE
            
          when others =>
            rd_state <= RD_IDLE;

        end case;
      end if;
    end if;
  end process;




end rtl;