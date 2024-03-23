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
        i_order_valid             : in std_logic_vector(3 downto 0);
        i_sendingTime                 : in std_logic_vector(167 downto 0);
        i_transactTime                : in std_logic_vector(143 downto 0)
    );

  end entity ipv4_header;

  architecture rtl of ipv4_header is



    -- b/c our order book will only monitor subset equities.
    -- and we only want a subset of commands/instructions/messages residing on the fpga for promptness.
    -- the algo needs to only set a few certain fields or tags, the rest should be preset and semi-deterministic.
    -- bram widths up to
    -- 512 Bytes, 4096 bits
    -- 256     2048 bits
    -- 128     1024 bits
    -- 64      512 bits
    
    -- strlen * 8 = num_bytes
    
    -- 4kx4, 2kx9, 1kx18, and 512x36 primitives these are bits!, 4k bits
    
    -- we can fit entire fix msg in BRAM. or atleast most of it. and then only updating time tags
    --     but we dont want to have redundant information. we store what is constant once. and call it.
    --     then we concatenate the time.
    
    --     anything that is constant or unchanging => it's partial sum should be calculated before handing.
    --     thus only updating partial sums when we input the time.
    
    --     furthermore for 8bit add, i think we can parallelize and tree down to a value. we dont have to fold like the tcp/ip, we dont care about excess 8bits.
    --     we dont care about the carries after the 7th bit, but we do care about the internal ones!
    
    -- maybe based on strategies =>  determined by devs =>  we keep the HIGHest risk orders in the fpga, with triggers set up. such that if we needed to cancel.
    -- the information is pre-populated. we know everything! including the length. everything except the time stamp of when this occurs.
    -- i think with the redundancy in order => we can read everything out in 1 cycle.
    -- we cna have [partial_sum][#bytes][msg] or something liek that in RAM. so you have information, and actual data.
    
    -- i imagine the algo has to look at specific symbols or buckets => you cant look at everything at once.
    -- you have to look at one symbol with many different variables that affect it., there for this symbol, its strategy purchase or whatever is known.
    -- or potential buy or cancel will reside in the ram. its location known by this algo. so say you are monitoring 20 symbols. you have 20algo engines. with accompanying RAM and potential buy, sell cancel change whatver =>
    
    -- i think the body len can be pre determined => because while the time is undertermined, it will use up the same amount of bytes.
    
    -- original => 
    -- 8=FIX.4.29=11135=F49=BBT56=OTHER34=452=20061124-16:38:47.09941=S1437=110967411=S151=28660=20061124-16:38:05
    -- 38 3d 46 49 58 2e 34 2e 32 01 39 3d 31 31 31 01 33 35 3d 46 01 34 39 3d 42 42 54 01 35 36 3d 4f 54 48 45 52 01 33 34 3d 34 01 35 32 3d 32 30 30 36 31 31 32 34 2d 31 36 3a 33 38 3a 34 37 2e 30 39 39 01 34 31 3d 53 31 34 01 33 37 3d 31 31 30 39 36 37 34 01 31 31 3d 53 31 35 01 31 3d 32 38 36 01 36 30 3d 32 30 30 36 31 31 32 34 2d 31 36 3a 33 38 3a 30 35 01
    
    -- 10=047     //not valid because i changed tags 49 56 sender and target.
    

    8=FIX.4.29=11135=F49=BBT56=OTHER34=4
    38 3d 46 49 58 2e 34 2e 32 01 39 3d 31 31 31 01 33 35 3d 46 01 34 39 3d 42 42 54 01 35 36 3d 4f 54 48 45 52 01 33 34 3d 34 01 
    len = 126/3 = 42 bytes
    left_partial sum = 0x89c
    
    41=S1437=110967411=S151=286
    34 31 3d 53 31 34 01 33 37 3d 31 31 30 39 36 37 34 01 31 31 3d 53 31 35 01 31 3d 32 38 36 01 
    len = 93/3 =31 byte
    right_partial sum = 0x5d7

    stored_partial_sum = 0x89c + 0x5d7 = 0xe73 "73"     -- this will already be in the ram/rom, precalculated
    
    -- 73bytes = 584,   + # byte separator + 2byte bot half length + 2 byte top half = 2 bytes partial sum = 6*8bit = 48bits,  1023 downto 976
    584 + 48 bits = 632, 1024-632 = 392..

    constant ZEROS : std_logic_vector(391 downto 0) := (others => '0');
    signal ram_out : std_logic_vector(1023 downto 0) := x"0E73_002A_001F" & ZEROS & x"383d4649582e342e3201393d3131310133353d460134393d4242540135363d4f544845520133343d340134313d5331340133373d313130393637340131313d53313501313d32383601";

    
    -- let..
    -- -- 20061124-16:38:47.099
    -- i_sendingTime = x"32 30 30 36 31 31 32 34 2d 31 36 3a 33 38 3a 34 37 2e 30 39 39";          -- 21bytes = 168bit -> 167 downto 0
    -- -- 20061124-16:38:05
    -- i_transactTime = x"32 30 30 36 31 31 32 34 2d 31 36 3a 33 38 3a 30 35";                     -- 18bytes, 144 -> 143 downto 0
    
    TAG_SENDINGTIME <= x"35 32 3d";       -- 52=
    TAG_TRANSACTTIME <= x"36 30 3d";      -- 60=
    TAG_FIXCHECKSUM <= x"31 30 3d";       -- 10=
    SOH <= x"01";
    
    process(clk) begin
        if(rising_edge(clk)) then
            if (i_order_valid) then
                sendingTime <= TAG_SENDINGTIME & i_sendingTime & SOH;           --3 + 22 + 1 = 26bytes
                -- 52=20061124-16:38:47.099
                -- 35 32 3d 32 30 30 36 31 31 32 34 2d 31 36 3a 33 38 3a 34 37 2e 30 39 39 01
    
                transactTime <= TAG_TRANSACTTIME & i_transactTime & SOH;        --3 + 18 + 1 = 22bytes
                -- 60=20061124-16:38:05
                -- 36 30 3d 32 30 30 36 31 31 32 34 2d 31 36 3a 33 38 3a 30 35 01
            end if;       
        end if;
    end process;
        
    fix_msg <= ram_out(42*8-1+31*8 downto 31*8) & sendingTime & ram_out(31*8-1 downto 0) & transactTime & TAG_FIXCHECKSUM & fix_checksum & x"01";
    
    
    -- for simple example =>  use ROM pre populate.
    -- calculate on arrival.
    -- the time is 26 + 22 = 48bytes, parallel adds =>  48>24>12>6>3>1.5>0 = 7 cycles
    -- 26, 13, 6.5, 3.25, 1.625, 0
    -- 22, 11, 5.5, 2.75, 1.375, 0
    -- they individually take6, but you have to combine for a 7th. so 7 either way.. 
    
    sendingTime checksum = 0x4e3
    transactTime checksum = 0x40c
    time_partial_sum = 0x8ef            --separate add is correct
    
    final_sum = stored_partial_sum + time_partial_sum = 0xe73 + 0x8ef = 0x1762
    -- 0x1762 from full fix msg => 
    
    fix_msg <= fix_msg & TAG_FIXCHECKSUM & final_sum & SOH;
    
    --double check 9, length of entire msg => 
    --time adder
    --state machine.
    --coordinate timing betwene fix, tcp and ip. determine when exactly to kick off.
    
    
    
    i think, because we have several "symbol/product" management with their own blocks monitoring everything separately.
    when algo determines hey do this, maybe that is when we set one of the time. TransactTime, 60 part of body
    it gets queud up in a fifo because we only have ONE TCP/IP ETH output
    when it is read out of process by FIFO , we update the final "send time" SendingTime, 52 part of header
    
    
    
    
    
    Example: Order Cancel Request
    --header
    8=FIX.4.2                      -- constant , len = 10          begin_string    : std_logic_vector(79 downto 0)     <= 38 3d 46 49 58 2e 34 2e 32 01;
    9=111                       --changes, len = 6                 body_len        : std_logic_vector(47 downto 0)     <= 39 3d    31 31 31 01;                --variable
    35=F                   --len=5                                 msg_type        : std_logic_vector(39 downto 0)     <= 33 35 3d 46 01;
    49=BBT                 --len=7                                 SenderCompID    : std_logic_vector(55 downto 0)     <= 34 39 3d 42 42 54 01;                -- we are always the sender..
    56=OTHER                          --len=9                       TargetCompID    : std_logic_vector()                <= 35 36 3d 4f 54 48 52 01              --variable string 32.
    34=4                                   --len= 5                MsgSeqNum       : std_logic_vector(31 downto 0)                <= pad zeross 33 34 3d 34 01
    52=20061124-16:38:47.099               --len = 25-3              SendingTime     : std_logic_vector(199 downto 0)    <= 35 32 3d 32 30 30 36 31 31 32 34 2d 31 36 3a 33 38 3a 34 37 2e 30 39 39 01   --variable
    -- above in ms
    
    -- body
    41=S14                         origclordid     : std_logic_vector(55 downto 0)            34 31 3d 53 31 34 01                                                 --7 	String(64) Last accepted ClOrdID in an order chain that this message modifies =>  does that mean we have been tracking this?
    37=1109674                     OrderID         : std_logic_vector(87 downto 0)            33 37 3d 31 31 30 39 36 37 34 01                                     --11      	String(32)  Unique identifier for order as assigned by the CQG gateway.
    11=S15                         ClOrdID         : std_logic_vector(55 downto 0)            31 31 3d 53 31 35 01                                                 --7          variable String(64) New and unique ID for this request.
    1=286                          account         : std_logic_vector(47 downto 0)            31 3d 32 38 36 01                                                    --6                String(256)
    60=20061124-16:38:05           transacttime    : std_logic_vector(167 downto 0)           36 30 3d 32 30 30 36 31 31 32 34 2d 31 36 3a 33 38 3a 30 35 01       --21-3        variable
    -- above in whole sec
    
    52  Time of message transmission (always expressed in UTC, Universal Time Coordinated, also known as GMT). Curre
    60  Time this order request was initiated/released by the trader or trading system.
    
    
    33 35 3d 46 01              0xec
    
    34 39 3d 66 69 78 5f 63 6c 69 65 6e 74 01       0x4d0
    
    5bc, example constant already
    
    we get the time input =>  36 30 3d 32 30 30 36 31 31 32 34 2d 31 36 3a 33 38 3a 30 35 01 =>  0x40c
    
    5bc + 40c =
    9c8, c8 = 9c8 good
    
    35 36 3d 43 51 47 5f 47 61 74 65 77 61 79 01 33 34 3d 34 01 35 32 3d 32 30 30 36 31 31 32 34 2d 31 36 3a 33 38 3a 34 37 2e 30 39 39 01 34 31 3d 53 31 34 01 33 37 3d 31 31 30 39 36 37 34 01 31 31 3d 53 31 35 01 31 3d 32 38 36 01 36 30 3d 32 30 30 36 31 31 32 34 2d 31 36 3a 33 38 3a 30 35 01
    sum = 0x1A10
    
    35=F49=fix_client56=CQG_Gateway34=4      --constant
    52=20061124-16:38:47.099                   --changing
    
    
    
    --body
    41=MDN137=0321202411=MDN11=71460=20061124-16:38:05
    
    34 31 3d 4d 44 4e 31 01 33 37 3d 30 33 32 31 32 30 32 34 01 31 31 3d 4d 44 4e 31 01 31 3d 37 31 34 01 36 30 3d 32 30 30 36 31 31 32 34 2d 31 36 3a 33 38 3a 30 35 01
    sum = 0xab0
    
    41=MDN137=0321202411=MDN11=714      -- constant
    60=20061124-16:38:05                   -- changing
    
    --trailer checksum
    10=047                                 -- changing
    
    to do:
    determine vector size for fields, just read the spec =>
    
    
    
    basically calculating the msg checksum is going to take the longest. but a decision has been made and can be forwarded
    to the TCP and IP blocks in which they calculate their partial header/checksum and go on standby.
    when FIX finishes its calculation, it passes this value to the TCP to complete it's checksum calc.
    somewhere during this time you can start sending the IP header to ETH MAC.
    by the time you transfer IP header 3cycles, you will have TCP ready and then another 3cycles.
    then start pumping the FIX msg out.
    
    
    
    
    
    
    
    The checksum must be calculated by summing the binary value of each octet from the start of the BeginString(8) field
    up to and including the end of field delimiter () of the field immediately preceding the CheckSum(10) field,
    then transforming this value using a modulo 256.
    
    
    
    
    for 8 bit adder you can use a 16bit register, but you dont really have to =>
    you can just keep adding the bytes anything that goes over 8 bit gets "truncated"
    anything over 8bits are the carries, which we truncate in the end anyways.
    
    
    
    
    ok so the FIX is add the bytes 8bit add. where as TCP and IP are 16bit adds.
    
    you can buffer the data one byte. to make 16bits and then pass that as the partial checksum to TCP
    
    ASK NING
    need to check why they use FIX over the other ones, what are pros cons.
    
    ASK BRIAN
    which market is BB already in and which are they trying to take a piece of.
    
    do you use SHA hash algorithms to create hash keys => . look up SHA first =>
    
    
    
  end rtl;


