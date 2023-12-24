library ieee;
use ieee.std_logic_1164.all;

package nasdaq_itch is

  -- Outputs from the FIFO.
  type t_FROM_FIFO is record
    wr_full  : std_logic;                -- FIFO Full Flag
    rd_empty : std_logic;                -- FIFO Empty Flag
    rd_dv    : std_logic;
    rd_data  : std_logic_vector(7 downto 0);
  end record t_FROM_FIFO;

  -- Inputs to the FIFO.
  type t_TO_FIFO is record
    wr_en    : std_logic;
    wr_data  : std_logic_vector(7 downto 0);
    rd_en    : std_logic;
  end record t_TO_FIFO;

  -- constant c_FROM_FIFO_INIT : t_FROM_FIFO := (wr_full => '0',
  --                                             rd_empty => '1',
  --                                             rd_dv => '0',
  --                                             rd_data => (others => '0'));

  -- constant c_TO_FIFO_INIT : t_TO_FIFO := (wr_en => '0',
  --                                         wr_data => (others => '0'),
  --                                         rd_en => '0');


end package nasdaq_itch;



-- ascii has 128 char, 7bits req
-- 1byte, 2hex, 8bits

-- assume ethernet output data = 32bit, 4bytes


-- ethernet_input_tdata

-- byte re-order when reassembling received bytes/data

  network_byte7_0 = eth_tdata_in[7:0];
  network_byte15_8 = eth_tdata_in[15:8];
  network_byte23_16 = eth_tdata_in[23:16];
  network_byte31_24 = eth_tdata_in[31:24];


host_byte_order = {network_byte7_0, network_byte15_8, network_byte23_16, network_byte31_24};

-- meant that MSB of host word was transfered first!.
-- we receive the MSB first and downwards


-- The TCP/IP standard network byte order is big-endian.
-- the lowest memory address to be the high-order bit, which is called big endian.
-- MSB to LSB as usual when we assign on host machine,
-- but network will pull the MSB to transfer first.


-- just enumerate ASCII somewhere else =>

stock_locate
tracking_num
timestamp
stock_symbol
event_code

-- https://www.nasdaqtrader.com/content/technicalsupport/specifications/dataproducts/NQTVITCHSpecification.pdf
  case msg_type is

    -- 1.1 System Event Message
    when S =>
        stock_locate
        tracking_num
        timestamp
        event_code

-- i dont know if we need this, they didn't have it. what determines what we need.
-- 1.2 Stock Related Messages
    -- 1.2.1 Stock Directory
    -- when R =>
    --   stock_locate
    --   tracking_no
    --   timestamp
    --   stock_symbol
    --   market_category
    --   FinancialStatusIndicator

    -- 1.2.2 Stock Trading Action
    when H =>
      stock_locate
      tracking_num
      timestamp
      stock_symbol
      trading_state

      -- no reserved or reason?

      (stock_locate uint16)
      (tracking uint16)
      (timestamp uint48)
      (symbol uint64)
      (trading-state uint8))


    -- 1.2.3 Reg SHO Short Sale Price Test RestrictedIndicator
    when Y =>
      stock_locate
      tracking_num
      timestamp
      stock_symbol
      reg_sho_action

-- i dont know if we need this, they didn't have it. what determines what we need.
    -- 1.2.4 Market Participant Position

    -- when L =>
    --   stock_locate
    --   tracking_no
    --   timestamp
    --   stock_symbol
    --   reg_sho_action

-- -- i dont know if we need this, they didn't have it. what determines what we need.
--     -- 1.2.5 Market---Wide Circuit Breaker (MWCB) Messaging
--     when V =>
--       code for this branch

-- -- i dont know if we need this, they didn't have it. what determines what we need.
--     -- 1.2.6 Quoting Period Update
--     when K =>
--       code for this branch

-- -- i dont know if we need this, they didn't have it. what determines what we need.
--     -- 1.2.7 Limit Up – Limit Down (LULD) Auction Collar
--     when J =>
--       code for this branch

--       -- i dont know if we need this, they didn't have it. what determines what we need.
--     -- 1.2.8 Operational Halt
--     when h =>
--       code for this branch

-- 1.3 Add Order Message
    -- 1.3.1 Add Order – No MPID Attribution
    when A =>
      stock_locate
      tracking_num
      timestamp
      order_ref_num
      buy_sell_ind
      num_of_shares
      stock_symbol
      price


      -- (65 add-order ; A
      -- (stock_locate uint16)
      -- (tracking uint16)
      -- (timestamp uint48)
      -- (order-ref-number uint64)
      -- (buy-sell uint8)
      -- (num-shares uint32)
      -- (symbol uint64)
      -- (price uint32))


    -- 1.3.2 Add Order with MPID Attribution
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

      -- (70 add-order-with-mpid ; F
      -- (stock_locate uint16)
      -- (tracking uint16)
      -- (timestamp uint48)
      -- (order-ref-number uint64)
      -- (buy-sell uint8)
      -- (num-shares uint32)
      -- (symbol uint64)
      -- (price uint32)
      -- (attribution uint32))


-- 1.4 Modify Order Messages
    -- 1.4.1 Order ExecutedMessage
    when E =>
      stock_locate
      tracking_num
      timestamp
      order_ref_num
      exec_num_shares
      match_num



      -- (69 order-executed ; E
      -- (stock_locate uint16)
      -- (tracking uint16)
      -- (timestamp uint48)
      -- (order-ref-number uint64)
      -- (num-shares uint32)
      -- (match-number uint64))


    -- 1.4.2 Order Executed With PriceMessage
    when C =>
      stock_locate
      tracking_num
      timestamp
      order_ref_num
      exec_num_shares
      match_num
      printable
      exec_price


      -- (67 order-executed-with-price ; C
      -- (stock_locate uint16)
      -- (tracking uint16)
      -- (timestamp uint48)
      -- (order-ref-number uint64)
      -- (num-shares uint32)
      -- (match-number uint64)
      -- (printable uint8)
      -- (price uint32))


    -- 1.4.3 Order Cancel Message
    when X =>
      stock_locate
      tracking_num
      timestamp
      order_ref_num
      num_share_cancel

      -- exec_num_shares they shared signal

      -- (88 order-cancel ; X
      -- (stock_locate uint16)
      -- (tracking uint16)
      -- (timestamp uint48)
      -- (order-ref-number uint64)
      -- (num-shares uint32))


    -- 1.4.4 Order Delete Message
    when D =>
      stock_locate
      tracking_num
      timestamp
      order_ref_num


      -- (68 order-delete ; D
      -- (stock_locate uint16)
      -- (tracking uint16)
      -- (timestamp uint48)
      -- (order-ref-number uint64))


    -- 1.4.5. Order Replace Message
    when U =>
      stock_locate
      tracking_num
      timestamp
      prev_order_ref_num
      order_ref_num    --new one
      exec_num_shares
      exec_price

      -- (85 order-replace ; U
      -- (stock_locate uint16)
      -- (tracking uint16)
      -- (timestamp uint48)
      -- (prev-order-ref-number uint64)
      -- (order-ref-number uint64)
      -- (num-shares uint32)
      -- (price uint32))



-- 1.5 Trade Messages
    -- 1.5.1 Trade Message (Non---Cross)
    when P =>
      stock_locate
      tracking_num
      timestamp
      order_ref_num    --new one
      buy_sell_ind
      exec_num_shares
      stock_symbol
      exec_price
      match_num

      -- (80 trade ; P
      -- (stock_locate uint16)
      -- (tracking uint16)
      -- (timestamp uint48)
      -- (order-ref-number uint64)
      -- (buy-sell uint8)
      -- (num-shares uint32)
      -- (symbol uint64)
      -- (price uint32)
      -- (match-number uint64))



    -- 1.5.2 Cross Trade Message
    when Q =>
      stock_locate
      tracking_num
      timestamp
      exec_num_shares
      stock_symbol
      exec_price
      match_num
      cross_type

      -- (81 cross-trade ; Q
      -- (stock_locate uint16)
      -- (tracking uint16)
      -- (timestamp uint48)
      -- (num-shares-msb uint32)
      -- (num-shares uint32)
      -- (price uint32)
      -- (match-number uint64)
      -- (cross-type uint8)



      
-- -- i dont know if we need this, they didn't have it. what determines what we need.
--     -- 1.5.3 Broken Trade / Order ExecutionMessage
--     when B =>
--       code for this branch


-- -- i dont know if we need this, they didn't have it. what determines what we need.
--     -- 1.6 Net Order Imbalance Indicator (NOII)Message
--     when I =>
--       code for this branch

--     -- 1.7 was grayed out

-- -- i dont know if we need this, they didn't have it. what determines what we need.
--     -- 1.8 Direct Listing with Capital Raise Price Discovery Message
--     when O =>
--       code for this branch

  when others =>

end case;





;; The general interface
(input uint32 (fpga-time "The FPGA time counter"))

;; The Ethernet AXI input stream interface
(input uint32 ethernet-input-tdata)
(input uint4 ethernet-input-tkeep)
(input bit ethernet-input-tlast ethernet-input-tvalid)
(output bit (ethernet-input-tready :special-use))

;; The commands AXI output stream interface
(input bit command-out-tready)
(output uint297 command-out-tdata)
(output bit command-out-tvalid)

;; Let's register the inputs
(setf ethernet-input-tlast (register ethernet-input-tlast))

;; Adding an extra clock cycle after the last word of each packet
(change-execution (after-packet :exec-when ethernet-input-tlast))

(setf (var bit start-of-packet) (falling-edge after-packet :initial-value 1))

;; The memory mapped registers interface
(def-mmap-interface config-registers "The config/status registers" :data-width 32 :nb-words 8)

(with-var-options (:interface config-registers)
  (input uint32 (nasdaq-ip-addr :untimed "The IP address of the incoming feed." :initial-value #.(ip32 233 54 12 101)))
  (input uint16 (nasdaq-udp-port :untimed "The IP port of the incoming feed." :initial-value 26400)))

;;The message parser takes the description of the messages and generates the hardware needed to decode them
(def-message-parser2 parser :data-valid (bit.and (bit.not after-packet) ethernet-input-tvalid)
                            :sop start-of-packet :tkeep ethernet-input-tkeep :data-in ethernet-input-tdata
                            :protocol-desc
(;; Ethernet header
   (dst-mac uint48)
   (src-mac uint48)
   (eth-type uint16)
;;IP header
   (version-and-IHL uint8)
   (DSCP-ECN uint8)
   (total-length uint16)
   (identification uint16)
   (flags-and-fragment-offset uint16)
   (time-to-live uint8)
   (protocol uint8)
   (header-checksum uint16)
   (ip-src-addr uint32)
   (ip-dest-addr uint32)
;; UDP header
   (udp-src-port int16)
   (udp-dest-port uint16)
   (udp-len uint16)
   (udp-checksum uint16)
;; MOLD header
   (mold-session uint64)
   (mold-session-msb uint16)
   (seqnum uint64)
   (msg-count uint16)






;; Computes the global message seqnum for each message
(def-counter seqnum32 32 :increment msg-type-sync :enable ethernet-input-tvalid :clear (delay seqnum-sync 3) :reset-value seqnum)

;; Only accepts the packets which don't have the correct IP addresse and port
(setf (var bit packet-ok) (bit.and (= ip-dest-addr nasdaq-ip-addr) (= udp-dest-port nasdaq-udp-port)))

;; Stores various event codes into num-shares to reduce the AXI stream width
(setf num-shares (case-expr msg-type
                            (83 event-code)
                            (72 trading-state)
                            (89 reg-sho-action)
                            (:default num-shares)))

;; Bundles the output into the command_out AXI4 stream data
(setf command-out-tdata (concat msg-type
                                order-ref-number
                                prev-order-ref-number
                                stock_locate
                                (= buy-sell #.(char-code #\B))
                                price
                                num-shares
                                seqnum32
                                timestamp))

(setf command-out-tvalid (bit.and packet-ok
                                  (bit.or price-sync event-code-sync trading-state-sync reg-sho-action-sync
                                          (bit.and (bit.or (= msg-type 69) (= msg-type 88)) num-shares-sync))))))
