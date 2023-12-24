library ieee;
use ieee.std_logic_1164.all;

package nasdaq_itch is

  __ Outputs from the FIFO.
  type t_FROM_FIFO is record
    wr_full  : std_logic;                __ FIFO Full Flag
    rd_empty : std_logic;                __ FIFO Empty Flag
    rd_dv    : std_logic;
    rd_data  : std_logic_vector(7 downto 0);
  end record t_FROM_FIFO;

  __ Inputs to the FIFO.
  type t_TO_FIFO is record
    wr_en    : std_logic;
    wr_data  : std_logic_vector(7 downto 0);
    rd_en    : std_logic;
  end record t_TO_FIFO;

  __ constant c_FROM_FIFO_INIT : t_FROM_FIFO := (wr_full => '0',
  __                                             rd_empty => '1',
  __                                             rd_dv => '0',
  __                                             rd_data => (others => '0'));

  __ constant c_TO_FIFO_INIT : t_TO_FIFO := (wr_en => '0',
  __                                         wr_data => (others => '0'),
  __                                         rd_en => '0');


end package nasdaq_itch;




-- The Ethernet AXI input stream interface
(input uint32 ethernet_input_tdata)
(input uint4 ethernet_input_tkeep)
(input bit ethernet_input_tlast ethernet_input_tvalid)
(output bit (ethernet_input_tready :special_use))

-- The commands AXI output stream interface
(input bit command_out_tready)
(output uint297 command_out_tdata)
(output bit command_out_tvalid)



__ ascii has 128 char, 7bits req
__ 1byte, 2hex, 8bits

__ assume ethernet output data = 32bit, 4bytes


__ ethernet_input_tdata

__ byte re_order when reassembling received bytes/data

  network_byte7_0 = eth_tdata_in[7:0];
  network_byte15_8 = eth_tdata_in[15:8];
  network_byte23_16 = eth_tdata_in[23:16];
  network_byte31_24 = eth_tdata_in[31:24];


host_byte_order = {network_byte7_0, network_byte15_8, network_byte23_16, network_byte31_24};

__ meant that MSB of host word was transfered first!.
__ we receive the MSB first and downwards


__ The TCP/IP standard network byte order is big_endian.
__ the lowest memory address to be the high_order bit, which is called big endian.
__ MSB to LSB as usual when we assign on host machine,
__ but network will pull the MSB to transfer first.


__ just enumerate ASCII somewhere else =>

stock_locate
tracking_num
timestamp
stock_symbol
event_code

__ https:--www.nasdaqtrader.com/content/technicalsupport/specifications/dataproducts/NQTVITCHSpecification.pdf
  case msg_type is

    __ 1.1 System Event Message
    when S =>
        stock_locate
        tracking_num
        timestamp
        event_code

__ i dont know if we need this, they didn't have it. what determines what we need.
__ 1.2 Stock Related Messages
    __ 1.2.1 Stock Directory
    __ when R =>
    __   stock_locate
    __   tracking_no
    __   timestamp
    __   stock_symbol
    __   market_category
    __   FinancialStatusIndicator

    __ 1.2.2 Stock Trading Action
    when H =>
      stock_locate
      tracking_num
      timestamp
      stock_symbol
      trading_state

      __ no reserved or reason?

      (stock_locate uint16)
      (tracking uint16)
      (timestamp uint48)
      (symbol uint64)
      (trading_state uint8))


    __ 1.2.3 Reg SHO Short Sale Price Test RestrictedIndicator
    when Y =>
      stock_locate
      tracking_num
      timestamp
      stock_symbol
      reg_sho_action

__ i dont know if we need this, they didn't have it. what determines what we need.
    __ 1.2.4 Market Participant Position

    __ when L =>
    __   stock_locate
    __   tracking_no
    __   timestamp
    __   stock_symbol
    __   reg_sho_action

__ __ i dont know if we need this, they didn't have it. what determines what we need.
__     __ 1.2.5 Market___Wide Circuit Breaker (MWCB) Messaging
__     when V =>
__       code for this branch

__ __ i dont know if we need this, they didn't have it. what determines what we need.
__     __ 1.2.6 Quoting Period Update
__     when K =>
__       code for this branch

__ __ i dont know if we need this, they didn't have it. what determines what we need.
__     __ 1.2.7 Limit Up – Limit Down (LULD) Auction Collar
__     when J =>
__       code for this branch

__       __ i dont know if we need this, they didn't have it. what determines what we need.
__     __ 1.2.8 Operational Halt
__     when h =>
__       code for this branch

__ 1.3 Add Order Message
    __ 1.3.1 Add Order – No MPID Attribution
    when A =>
      stock_locate
      tracking_num
      timestamp
      order_ref_num
      buy_sell_ind
      num_of_shares
      stock_symbol
      price


      __ (65 add_order ; A
      __ (stock_locate uint16)
      __ (tracking uint16)
      __ (timestamp uint48)
      __ (order_ref_number uint64)
      __ (buy_sell uint8)
      __ (num_shares uint32)
      __ (symbol uint64)
      __ (price uint32))


    __ 1.3.2 Add Order with MPID Attribution
    when F =>
      stock_locate
      tracking_num
      timestamp
      order_ref_num
      buy_sell_ind
      num_of_shares
      stock_symbol
      price
      attribution

      __ (70 add_order_with_mpid ; F
      __ (stock_locate uint16)
      __ (tracking uint16)
      __ (timestamp uint48)
      __ (order_ref_number uint64)
      __ (buy_sell uint8)
      __ (num_shares uint32)
      __ (symbol uint64)
      __ (price uint32)
      __ (attribution uint32))


__ 1.4 Modify Order Messages
    __ 1.4.1 Order ExecutedMessage
    when E =>
      stock_locate
      tracking_num
      timestamp
      order_ref_num
      exec_num_shares
      match_num



      __ (69 order_executed ; E
      __ (stock_locate uint16)
      __ (tracking uint16)
      __ (timestamp uint48)
      __ (order_ref_number uint64)
      __ (num_shares uint32)
      __ (match_number uint64))


    __ 1.4.2 Order Executed With PriceMessage
    when C =>
      stock_locate
      tracking_num
      timestamp
      order_ref_num
      exec_num_shares
      match_num
      printable
      exec_price


      __ (67 order_executed_with_price ; C
      __ (stock_locate uint16)
      __ (tracking uint16)
      __ (timestamp uint48)
      __ (order_ref_number uint64)
      __ (num_shares uint32)
      __ (match_number uint64)
      __ (printable uint8)
      __ (price uint32))


    __ 1.4.3 Order Cancel Message
    when X =>
      stock_locate
      tracking_num
      timestamp
      order_ref_num
      num_share_cancel

      __ exec_num_shares they shared signal

      __ (88 order_cancel ; X
      __ (stock_locate uint16)
      __ (tracking uint16)
      __ (timestamp uint48)
      __ (order_ref_number uint64)
      __ (num_shares uint32))


    __ 1.4.4 Order Delete Message
    when D =>
      stock_locate
      tracking_num
      timestamp
      order_ref_num


      __ (68 order_delete ; D
      __ (stock_locate uint16)
      __ (tracking uint16)
      __ (timestamp uint48)
      __ (order_ref_number uint64))


    __ 1.4.5. Order Replace Message
    when U =>
      stock_locate
      tracking_num
      timestamp
      prev_order_ref_num
      order_ref_num    __new one
      exec_num_shares
      exec_price

      __ (85 order_replace ; U
      __ (stock_locate uint16)
      __ (tracking uint16)
      __ (timestamp uint48)
      __ (prev_order_ref_number uint64)
      __ (order_ref_number uint64)
      __ (num_shares uint32)
      __ (price uint32))



__ 1.5 Trade Messages
    __ 1.5.1 Trade Message (Non___Cross)
    when P =>
      stock_locate
      tracking_num
      timestamp
      order_ref_num    __new one
      buy_sell_ind
      exec_num_shares
      stock_symbol
      exec_price
      match_num

      __ (80 trade ; P
      __ (stock_locate uint16)
      __ (tracking uint16)
      __ (timestamp uint48)
      __ (order_ref_number uint64)
      __ (buy_sell uint8)
      __ (num_shares uint32)
      __ (symbol uint64)
      __ (price uint32)
      __ (match_number uint64))



    __ 1.5.2 Cross Trade Message
    when Q =>
      stock_locate
      tracking_num
      timestamp
      exec_num_shares
      stock_symbol
      exec_price
      match_num
      cross_type

      __ (81 cross_trade ; Q
      __ (stock_locate uint16)
      __ (tracking uint16)
      __ (timestamp uint48)
      __ (num_shares_msb uint32)
      __ (num_shares uint32)
      __ (price uint32)
      __ (match_number uint64)
      __ (cross_type uint8)



      
__ __ i dont know if we need this, they didn't have it. what determines what we need.
__     __ 1.5.3 Broken Trade / Order ExecutionMessage
__     when B =>
__       code for this branch


__ __ i dont know if we need this, they didn't have it. what determines what we need.
__     __ 1.6 Net Order Imbalance Indicator (NOII)Message
__     when I =>
__       code for this branch

__     __ 1.7 was grayed out

__ __ i dont know if we need this, they didn't have it. what determines what we need.
__     __ 1.8 Direct Listing with Capital Raise Price Discovery Message
__     when O =>
__       code for this branch

  when others =>

end case;





-- The general interface
(input uint32 (fpga_time "The FPGA time counter"))



-- Let's register the inputs
(setf ethernet_input_tlast (register ethernet_input_tlast))

-- Adding an extra clock cycle after the last word of each packet
(change_execution (after_packet :exec_when ethernet_input_tlast))

(setf (var bit start_of_packet) (falling_edge after_packet :initial_value 1))

-- -- The memory mapped registers interface
(def_mmap_interface config_registers "The config/status registers" :data_width 32 :nb_words 8)

(with_var_options (:interface config_registers)
  (input uint32 (nasdaq_ip_addr :untimed "The IP address of the incoming feed." :initial_value #.(ip32 233 54 12 101)))
  (input uint16 (nasdaq_udp_port :untimed "The IP port of the incoming feed." :initial_value 26400)))

-- --The message parser takes the description of the messages and generates the hardware needed to decode them
(def_message_parser2 parser :data_valid (bit.and (bit.not after_packet) ethernet_input_tvalid)
                            :sop start_of_packet :tkeep ethernet_input_tkeep :data_in ethernet_input_tdata
                            :protocol_desc

(-- Ethernet header - MAC - data link layer
   (dst_mac uint48)
   (src_mac uint48)
   (eth_type uint16)

   -- coming out of the ethernet, i shouldn't have to care about the above?
   -- what 

--IP header           -- IP - net work layer
   (version_and_IHL uint8)
   (DSCP_ECN uint8)
   (total_length uint16)
   (identification uint16)
   (flags_and_fragment_offset uint16)
   (time_to_live uint8)
   (protocol uint8)
   (header_checksum uint16)
   (ip_src_addr uint32)
   (ip_dest_addr uint32)

-- UDP header               -- Transport
   (udp_src_port int16)
   (udp_dest_port uint16)
   (udp_len uint16)
   (udp_checksum uint16)

-- MOLD header            -- nasdaqs's moldudp64, on top of the udp
   (mold_session uint64)
   (mold_session_msb uint16)
   (seqnum uint64)
   (msg_count uint16)






-- Computes the global message seqnum for each message
(def_counter seqnum32 32 :increment msg_type_sync :enable ethernet_input_tvalid :clear (delay seqnum_sync 3) :reset_value seqnum)

-- __ this is confusing =>  why?
-- Only accepts the packets which don't have the correct IP addresse and port
(setf (var bit packet_ok) (bit.and (= ip_dest_addr nasdaq_ip_addr) (= udp_dest_port nasdaq_udp_port)))

-- __ not sure im going to do this, since i wasn't putting into axi stream
-- Stores various event codes into num_shares to reduce the AXI stream width
(setf num_shares (case_expr msg_type
                            (83 event_code)
                            (72 trading_state)
                            (89 reg_sho_action)
                            (:default num_shares)))

-- Bundles the output into the command_out AXI4 stream data
(setf command_out_tdata (concat msg_type
                                order_ref_number
                                prev_order_ref_number
                                stock_locate
                                (= buy_sell #.(char_code #\B))
                                price
                                num_shares
                                seqnum32
                                timestamp))


(setf command_out_tvalid (bit.and packet_ok
                                  (bit.or price_sync event_code_sync trading_state_sync reg_sho_action_sync
                                          (bit.and (bit.or (= msg_type 69) (= msg_type 88)) num_shares_sync))))))
