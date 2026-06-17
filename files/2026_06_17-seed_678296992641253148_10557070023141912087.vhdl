-- Seed: 678296992641253148,10557070023141912087

library ieee;
use ieee.std_logic_1164.all;

entity y is
  port (hxctgitep : out bit; sc : buffer std_logic_vector(4 to 3); wuc : inout std_logic_vector(0 downto 4); kca : inout bit_vector(1 downto 3));
end y;

architecture sjry of y is
  
begin
  -- Single-driven assignments
  kca <= (others => '0');
  hxctgitep <= '0';
  
  -- Multi-driven assignments
  wuc <= "";
end sjry;

library ieee;
use ieee.std_logic_1164.all;

entity iw is
  port (nuznncb : out integer; wr : in integer; czsn : inout std_logic_vector(3 to 2));
end iw;

library ieee;
use ieee.std_logic_1164.all;

architecture tb of iw is
  signal usjn : bit_vector(1 downto 3);
  signal ryzlbj : bit;
  signal wowaiaka : bit_vector(1 downto 3);
  signal hnbdwxlra : bit;
  signal gytvxbsg : bit_vector(1 downto 3);
  signal foogsf : std_logic_vector(0 downto 4);
  signal zyhrwr : std_logic_vector(4 to 3);
  signal znwdock : bit;
  signal odc : bit_vector(1 downto 3);
  signal ytoybcsvj : std_logic_vector(0 downto 4);
  signal cspnsmrb : bit;
begin
  noplowwy : entity work.y
    port map (hxctgitep => cspnsmrb, sc => ytoybcsvj, wuc => czsn, kca => odc);
  h : entity work.y
    port map (hxctgitep => znwdock, sc => zyhrwr, wuc => foogsf, kca => gytvxbsg);
  edlbzjr : entity work.y
    port map (hxctgitep => hnbdwxlra, sc => czsn, wuc => foogsf, kca => wowaiaka);
  wnqzrwt : entity work.y
    port map (hxctgitep => ryzlbj, sc => ytoybcsvj, wuc => ytoybcsvj, kca => usjn);
  
  -- Single-driven assignments
  nuznncb <= 16#02A#;
  
  -- Multi-driven assignments
  ytoybcsvj <= "";
  zyhrwr <= (others => '0');
  foogsf <= (others => '0');
end tb;

entity tmn is
  port (wzk : out integer; mwjfhzzwbx : linkage time);
end tmn;

architecture zt of tmn is
  
begin
  -- Single-driven assignments
  wzk <= 11;
end zt;

library ieee;
use ieee.std_logic_1164.all;

entity yltx is
  port (qbk : linkage std_logic_vector(4 to 1); ccwdyij : inout real; ianvwzi : linkage integer; cswygndkd : linkage real);
end yltx;

library ieee;
use ieee.std_logic_1164.all;

architecture z of yltx is
  signal bkrryxkai : bit_vector(1 downto 3);
  signal n : std_logic_vector(0 downto 4);
  signal tlhd : std_logic_vector(4 to 3);
  signal me : bit;
  signal loftoaamqp : bit_vector(1 downto 3);
  signal opjh : std_logic_vector(0 downto 4);
  signal rv : bit;
begin
  am : entity work.y
    port map (hxctgitep => rv, sc => opjh, wuc => opjh, kca => loftoaamqp);
  yjail : entity work.y
    port map (hxctgitep => me, sc => tlhd, wuc => n, kca => bkrryxkai);
  
  -- Single-driven assignments
  ccwdyij <= 3_0.3003;
  
  -- Multi-driven assignments
  opjh <= (others => '0');
  opjh <= "";
  opjh <= "";
end z;



-- Seed after: 4902141614519150317,10557070023141912087
