
entity ipv4_checksum_checker

clk : in std_logic;
rst : in std_logic;
--after ETH MAC parses/collect data, 32bit is out -- the first 5 words
i_data_in       : in std_logic_vector(64 downto 0);
i_data_valid    : in std_logic;                          --header valid.
-- ASSUME valid will be asserted for entirety of IP packet.. but we only care about first 5words, 20bytes.
o_ipv4_header_good : out std_logic;

end entity;

-- can store in 32 and then do top half + bottom half at the end.
signal sum : std_logic_vector(19 downto 0); -- make wider to capture carry

-- 20bytes, 32bit mac, 4bytes per cycle =>  5 cycles for complete header
process(clk) begin
if (rising_edge(clk)) then

    if(rst = '1' or data_valid = '0') then
        sum <= 0;

    elsif (data_valid = '1') then
        if (word_cnt < 5) then       -- cycle 0 1 2 3 4
            sum <= sum + data_in(31 downto 16) + data_in(15 downto 0);
        end if;
    end if;

    if (rst = '1' or data_valid = '0') then
        word_cnt <= 0;
    elsif(data_valid = '1') then
        if word_cnt < 5 then            -- i only need to go from 0 to 4, but i am letting it go to 5 so that i only pulse checksum valid.
            word_cnt <= word_cnt + 1;
        end if;
    end if;

    if finalsum = x"ffff" then
        checksum_good <= '1';
    else
        checksum_good <= '0';
    end if;
end if;
end process;


process (all) begin
if word_cnt = 5 then
    finalsum <= sum(15 downto 0) + x"000" & sum(19 downto 16); -- add carry/ or carry correction.
end if;
end process;

o_ipv4_header_good <= checksum_good;

--note to self
-- 0 1 2 3 4. during 2nd, do not use checksum in calc.
when creating the check sum / transmitting. we already know the source address and dest.
so we have all the info we need to create checksum.
well we probably have to wait for total length and time to live.



i see the IP header is checked above and also used for tcp checksum, so checked twice.
do you find that necessary? basically the pseudo IP header



for simplicity i am assuming NO options.
so we get the IP coming out from the MAC.
store the
src address 32bit
dst address 32bit
zeros, protcol. tcp length = IP length - 2(20B) = IPlen - 40B, the 40B is for the IP header and TCP header.

The TCP length field is the length of the TCP header and data (measured in octets). so only =>  IPlen - 20B for its header. (and any options)

from the above code and the IP header =>
0 capture, data_in(15 downto 0) = total length.. tf. TCPlen = this - 20B, length is 2bytes
        IHL, data_in(27 downto 24), 4bits, means up to 15 extra cycles, ea cycle =4bytes =>  4*15=60B IP header =>  before we get TCP stuff
1 nothing
2 capture, data(23 downto 16 ) is the protocol.., 1byte
3 capture, src IP, 4bytes
4 capture, dest IP, 4bytes
PSEUDO header is 11bytes long, IT DOESNT CARE ABOUT OPTION!!!

then you start to parse the TCP..
check data offset if there are options (header extensions) , if even, LSB = 0, then header is ODD byte count.
if odd, LSB = 1, then headed will have EVEN byte count =>  because pseuedo header is odd, TCP header is 20byte (even)
odd + even = odd + option(even/odd)



note the TCP checksum approach is similar to the IP checksum in that
is is 16bit 1's comp addition. carry correction at the end.
the IP only checks the header.. while the TCP uses the IP and checks the TCP header + its data.


This checksum field is assumed to be all zeroes during calculation of the checksum.

This special TCP checksum algorithm was eventually also adopted for use by the User Datagram Protocol (UDP).

remember TCP is segment. IP packet. ethernet frame. UDP datagram.

the TCP checksum should start/kickoff at the same time the IP checksum starts


IP options, look at IHL, byte 0, 4-7 which means in std_logic_vector.. 27 downto 24. (version is 31 downto 28)
for TCP options.. look at dataoffset, 4 count, 31 downto 28.










what if i make a simple app..

im going to take asimple FIX message in C,
have C talk to HDL
pass FIX data to TCP.
send TCP to IP , do all the header stuff for it.

validate in questa/modelsim, capture waveforms.

see how jinja2 applies to this (python)

push all onto git, for ning to review or checkout.



-- signal ip_hdr_0 : std_logic_vector(31 downto 0);
-- signal ip_hdr_1 : std_logic_vector(31 downto 0);
-- signal ip_hdr_2 : std_logic_vector(31 downto 0);
-- sum0 <= ip_hdr_0 + ip_hdr_1;                        --32 32 into 48
-- sum1 <= src_ip_addr + dest_ip_addr;                 --32 32 into 48

-- sum01 <= sum0 + sum1;           -- 48s into 48

-- sum2 <= sum01(47 downto 32) + sum01(31 downto 0);        -- take care of carry, now 32b

-- sum3 <= sum01(31 downto 16) + sum01(15 downto 0);        -- fold 16s into now 20b

-- sum4 <= sum3 + x"000" & protocol;                     --still 20b



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




--   process(clk) begin
--     case (state) is
--         when IDLE =>
--             if (enable) then
--                 sum2 <= sum01 + x"0000" & ip_hdr_2;
--                 state <= ST1;
--             end if;

--         when ST1 =>
--             -- fold to create 16bit value
--             sum3 <= sum2(31 downto 16) + sum2(15 downto 0);
--             state <= ST2;

--         when ST2 =>
--             -- add carry
--             sum4 <= x"000" & sum3(19 downto 16) + sum3(15 downto 0);
--             state <= ST3;

--         when ST3 =>
--             ip_hdr_checksum <= sum4 + sum2(47 downto 32);

--             if(done) then
--                 state <= IP_HDR_OUTPUT;
--             end if;
--         when others =>
--             state <= IDLE;

-- end process;



-- -- DSP is 48bit accumulator, making it 64 will bleed and probably not be efficient, you can try after but do 32bit first.
-- -- but they can be cascaded =>  look into after.

-- because equal to 8bytes, and header is 20bytes =>  3cycles to send all of header =>
-- but you need the check sum calculated before the 2nd cycle =>
--64bit
ip_hdr_0 <= version & ihl & dscp & ecn & total_length & identification & flags & fragment_offset;
ip_hdr_1 <= time_to_live & protocol & x"0000" & src_ip_addr;

(67 downto 0) 64bit add
sum0 <= ip_hdr_0 + ip_hdr_1;

fold 32bit add, take care of carry
sum1 <= sum0(63 downto 32) + sum0(31 downto 0) + x"0000_000" & sum0(67 downto 64);

sum2 <= sum1 + dest_ip_addr;

sum3 <= sum2(31 downto 16) + sum2(15 downto 0) + x"00" & sum2(39 downto 32);

in both cases =>  it takes 4 cycles

20bytes of header info.


i dont think you want to fold =>  bc then it requires an extra cycle










--16bit reg
ip_hdr_00 <= version & ihl & dscp & ecn;
ip_hdr_01 <= total_length;
sum0

ip_hdr_10 <= identification;
ip_hdr_11 <= flags & fragment_offset;
sum1

ip_hdr_20 <= time_to_live & protocol;
-- 21 DNE because all zeros
sum2

ip_hdr_30 <= src_ip_addr(31 downto 16);
ip_hdr_31 <= src_ip_addr(15 downto 0);
sum3

ip_hdr_40 <= dest_ip_addr(31 downto 16);
ip_hdr_41 <= dest_ip_addr(15 downto 0);
sum4


2nd clk
sum01 <= sum0 + sum1;
sum23 <= sum2 + sum3;

3rd clk
sum0123 <= sum01 + sum23

4th clk
sum0123+sum4

carry



20/2 = 10
10/2 = 5
5/2 = 2.5
2.5/2 = 1.25
1.25 /2 = 0.625 stop
take care of carry.

the problem is to reduce 10




the data can be large, there must be a top level TCP with controls/state machine, to break the
large data into smaller "segments"

when data (FIX string or whatever) is presented to TCP block, the length is known and given.
for simplicity the TCP header will not use options, tf 20Byte.

for us to send the TCP header, means the checksum is calculated, means we have to have already received ALL the data.
ALL THE DATA =>
    bc the checksum is over the TCP header + all the data + pseudo IP header!


so we want to send a tcp. and have the data, the application says the length is whatever. and starts giving data to TCP module =>
as the data comes in checksum is being calculated. data is stored in a buffer.
bc we are sender, we know the source address and destination =>  and legnth.


i assume many of the ipv4 header fields are constant. or can be determined before arrival
of TCP data. such that part of the check sum is already calculated.


TCP payload arrives, data_valid asserted at IP block.
next cycle IP block sends out 64b of the header.. to eth mac



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

-- you need to make the output combinatorial or zero delay




    case (state_hdr) is
        when HDR_IDLE =>
            if (data_in_valid) then
                --ST0, 32bit add, pseudo IP header + TCP header
                sum0 <= src_ip_addr                             + dest_ip_addr;                 -- pseudo header
                sum1 <= x"00" & ip_protocol & tcp_len           + src_port & dest_port;         -- LHS pseudo header, RHS TCP
                sum2 <= seq_num                                 + ack_num;
                sum3 <= data_offset & x"0" & flags & window     + x"0000" & urg_ptr;            -- TCP checksum set to 0, for sender calc.

                state_hdr <= ST0;
                -- if we receive a data valid, with nothing to keep and last asserted, and len = 0
                -- we will continue calculating the header for a TCP segment of 0 length.
                -- we will still send the header. just w no data
            end if;

        when ST1 =>
            sum01 <= sum0 + sum1;
            sum23 <= sum2 + sum3;

            state_hdr <= ST2;

        when ST2 =>
            sum0123 <= sum01 + sum23;

            if (sum0123(35 downto 32) > 0 ) then
                state_hdr <= ST3;
            else
                state_hdr <= ST4;
            end if;

        when ST3 => --carry
            sum0123 <= x"0000_000" & sum0123(35 downto 32) + sum0123(31 downto 0);
            state_hdr <= ST4;

        when ST4 => --fold
            sum_fold <= sum0123(31 downto 16) + sum0123(15 downto 0);

            if sum_fold(19 downto 16) > 0 then
                state_hdr <= ST5;
            else
                if tcp_len < 40 cycles then,     -- each cycle is 8bytes in =>  means data is greater than 48bytes
                    --done
                    state_hdr <= combine_checksum;
                else
                    --go to wait state_hdr, wait for data checksum calc.
                    state_hdr <= ST6;
                end if;
            end if;

        when ST5 => --carry
            tcp_hdr_checksum <= x"000" & sum_fold(19 downto 16) + sum_fold(15 downto 0);

            if tcp_len < 48 cycles then,     -- each cycle is 8bytes in =>  means data is greater than 48bytes
                --done
                state_hdr <= combine_checksum;
            else
                --go to wait state_hdr, wait for data checksum calc.
                state_hdr <= ST6;
            end if;

        when ST6 =>
            if (data_done) then
                state_hdr <= combine_checksum;
            end if;
            -- just stay here and let tcp calculate data checksum
            -- when data is done it still needs to fold 64 --> 32 --> 16 before it can be combined with the hdr checksum

        when combine_checksum =>


-- as data comes in, we collect it in a FIFO AND calculate the checksum on the data.
-- associative and commutive properties allow us to add out of order.
-- we dont need to pass the data to IP block bc it doesn't do anything with it.
-- the IP block will just need to calculate its header and then send it to eth mac.
-- after header is sent then the data. which means just read from the FIFO.

-- we will know the length ahead of time.
case (state_data) is
    when DATA_IDLE =>
        if (data_in_valid)              -- you need to use the last and keep.
            sum_data <= data_in;
            -- these are 64bit adds! sum_data probably should be greater than 68bits to be safe/handle the carry accumulation..

            if (tcp_len > 8bytes ) then
                state_data <= DATA_ACCUM;
            else
                --done, check carry and fold, check carry again.

            end if;
        end if;

    when DATA_ACCUM =>
        if (data_in_last), something keep then

            sum_data <= sum_data + data_in;
            state_data <=

        else
            sum_data <= sum_data + data_in;
        end if;


if sum_data(67 downto 64) > 0 then
    sum_data_fold32  <= sum_data(67 downto 64) + sum_data;

    sum_data_fold32  <= sum_data(63 downto 32) + sum_data(31 downto 0);
end if;

if sum_data_fold(35 downto 32) > 0 then
    sum_data_fold <= sum_data_fold(35 downto 32) + sum_data_fold
else
    sum_data_fold <=

tcp_data_checksum

-- these are 64bit adders =>  towards the end you will need to fold and take care of the carries.

if (data_in_valid) then
    fifo(index) <= data_in;
end if;

if (data_in_valid) then
    index <= index + 1;
end if;
--index should count up and match tcp len - 20 byte header.
-- because we know the len, in the ip block =>  we know how many times to read the fifo
-- we will need to track full so that we do not overflow. like wise empty, so we dont read invalid data.

end rtl;

note what if length is 20, ie just tcp header, no data.


i think when application says hey we want to do a TCP or even in fpga if we want to do TCP,
we know the next thing is going to be sending it to IP block =>  so the second we get valid data for TCP
start calculating the IP header in parallel for what you can.
things like time to live, or whatever else can wait. til we're actually ready to send.
but essentially everything else is "constant"







Example: Order Cancel Request

8=FIX.4.29=11135=F49=fix_client56=CQG_Gateway34=452=20061124-16:38:47.09941=S1437=110967411=S151=28660=20061124-16:38:0510=047 

fix 4.2

standard header
8 beginstring       66 69 78 34 2e 32       --FIX4.2
9 bodylength
35 msgtype              F (cancel order),
49 sendercompID
56 targetcompID
34 msgseqnum
52 sendingtime

msg body
1 account*
11 ClOrdID
37 OrderID*
41 origclordid

55 symbol not req
54 side not req => 
60 transacttime

trailer
10 checksum

SESSION vs APPLICATION
session is like starting it, logon/logoff.
application is actual market stuff we'll be doing.
so you'll have to do session first to make establish a connection..


Example: New Order Single

8=FIX.4.29=14135=D49=fix_client56=CQG_Gateway34=452=20061124-15:51:12.0931=28611=MS2455=F.US.TYAZ0654=260=20061124-15:50:5138=540=444=1.2277=O20154=S99=1.2410=045 


standard header
8 beginstring       46 49 58 2e 34 2e 32       --FIX4.2
9 bodylength            Message length, in bytes, f
35 msgtype              D ( order single),
49 sendercompID
56 targetcompID
34 msgseqnum
52 sendingtime

msg body
1 account
11 clordid
21 handlinst
55 symbol
54 side                 eg. 1 = buy, 2 = sell, 1-9
60 transacttime
38 OrderQty
40 ordtype

trailer
10 checksum


--header
8=FIX.4.2     
9=49             
length is everything after the length field EXCLUDING the cheksum

35=5                             5
49=BBT                           8
56=TW                             6
34=1                           5
52=20150916-04:14:05.306             25
stop checksum

--in this ex. no body.

--trailer
10=157

38 3d 46 49 58 2e 34 2e 32 7c
39 3d 34 39 7c
33 35 3d 35 7c
33 34 3d 31 7c
34 39 3d 41 52 43 41 7c
35 32 3d 32 30 31 35 30 39 31 36 2d 30 34 3a 31 34 3a 30 35 2e 33 30 36 7c
35 36 3d 54 57 7c



if we're going to send the msg, we need to know the length ahead of time.


checksum 
subtract or replace the 7c with 0x01, ascii for SOH. the delimiter, there are 7x 7c so +7
= 0xc96 = 3222 + 7 = 3229 mod 256 = 157 decimal -> 31 30 3d 31 35 37 01

Three byte, simple checksum.
31 35 37




b/c our order book will only monitor subset equities.
and we only want a subset of commands/instructions/messages residing on the fpga for promptness.
the algo needs to only set a few certain fields or tags, the rest should be preset and semi-deterministic.
bram widths up to 
512 Bytes, 4096 bits
256     2048 bits
128     1024 bits
64      512 bits

strlen * 8 = num_bytes

4kx4, 2kx9, 1kx18, and 512x36 primitives these are bits!, 4k bits

we can fit entire fix msg in BRAM. or atleast most of it. and then only updating time tags 
    but we dont want to have redundant information. we store what is constant once. and call it.
    then we concatenate the time.

    anything that is constant or unchanging => it's partial sum should be calculated before handing.
    thus only updating partial sums when we input the time.

    furthermore for 8bit add, i think we can parallelize and tree down to a value. we dont have to fold like the tcp/ip, we dont care about excess 8bits.
    we dont care about the carries after the 7th bit, but we do care about the internal ones!
    
maybe based on strategies =>  determined by devs =>  we keep the HIGHest risk orders in the fpga, with triggers set up. such that if we needed to cancel.
the information is pre-populated. we know everything! including the length. everything except the time stamp of when this occurs.
i think with the redundancy in order => we can read everything out in 1 cycle.
we cna have [partial_sum][#bytes][msg] or something liek that in RAM. so you have information, and actual data.    

i imagine the algo has to look at specific symbols or buckets => you cant look at everything at once.
you have to look at one symbol with many different variables that affect it., there for this symbol, its strategy purchase or whatever is known.
or potential buy or cancel will reside in the ram. its location known by this algo. so say you are monitoring 20 symbols. you have 20algo engines. with accompanying RAM and potential buy, sell cancel change whatver => 

i think the body len can be pre determined => because while the time is undertermined, it will use up the same amount of bytes.

8=FIX.4.29=11135=F49=fix_client56=CQG_Gateway34=452=20061124-16:38:47.09941=S1437=110967411=S151=28660=20061124-16:38:0510=047
38 3d 46 49 58 2e 34 2e 32 01 39 3d 31 31 31 01 33 35 3d 46 01 34 39 3d 66 69 78 5f 63 6c 69 65 6e 74 01 35 36 3d 43 51 47 5f 47 61 74 65 77 61 79 01 33 34 3d 34 01 35 32 3d 32 30 30 36 31 31 32 34 2d 31 36 3a 33 38 3a 34 37 2e 30 39 39 01 34 31 3d 53 31 34 01 33 37 3d 31 31 30 39 36 37 34 01 31 31 3d 53 31 35 01 31 3d 32 38 36 01 36 30 3d 32 30 30 36 31 31 32 34 2d 31 36 3a 33 38 3a 30 35 01 31 30 3d 30 34 37 01

fix_msg <= ram_out(41*8-1+31*8 downto 31*8) & SendingTime & ram_out(31*8-1 downto 0) & transacttime

Example: Order Cancel Request
--header
8=FIX.4.2                      -- constant , len = 10          begin_string    : std_logic_vector(79 downto 0)     <= 38 3d 46 49 58 2e 34 2e 32 01;
9=111                       --changes, len = 6                 body_len        : std_logic_vector(47 downto 0)     <= 39 3d    31 31 31 01;                --variable
35=F                   --len=5                                 msg_type        : std_logic_vector(39 downto 0)     <= 33 35 3d 46 01;
49=BBT                 --len=7                                 SenderCompID    : std_logic_vector(55 downto 0)     <= 34 39 3d 42 42 54 01;                -- we are always the sender..
56=OTHR                          --len=8                       TargetCompID    : std_logic_vector()                <= 35 36 3d 4f 54 48 52 01              --variable string 32.
34=4                                   --len= 5                MsgSeqNum       : std_logic_vector(31 downto 0)                <= pad zeross 33 34 3d 34 01
52=20061124-16:38:47.099               --len = 25              SendingTime     : std_logic_vector(199 downto 0)    <= 35 32 3d 32 30 30 36 31 31 32 34 2d 31 36 3a 33 38 3a 34 37 2e 30 39 39 01   --variable
-- above in ms

-- body 
41=S14                         origclordid     : std_logic_vector(55 downto 0)            34 31 3d 53 31 34 01                                                 --7 	String(64) Last accepted ClOrdID in an order chain that this message modifies =>  does that mean we have been tracking this?
37=1109674                     OrderID         : std_logic_vector(87 downto 0)            33 37 3d 31 31 30 39 36 37 34 01                                     --11      	String(32)  Unique identifier for order as assigned by the CQG gateway.
11=S15                         ClOrdID         : std_logic_vector(55 downto 0)            31 31 3d 53 31 35 01                                                 --7          variable String(64) New and unique ID for this request.
1=286                          account         : std_logic_vector(47 downto 0)            31 3d 32 38 36 01                                                    --6                String(256)
60=20061124-16:38:05           transacttime    : std_logic_vector(167 downto 0)           36 30 3d 32 30 30 36 31 31 32 34 2d 31 36 3a 33 38 3a 30 35 01       --21        variable
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











