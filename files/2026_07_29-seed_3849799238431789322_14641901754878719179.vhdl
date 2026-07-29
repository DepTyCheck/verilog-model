-- Seed: 3849799238431789322,14641901754878719179

library ieee;
use ieee.std_logic_1164.all;

entity hczhvxhxbg is
  port (uguccjzj : linkage std_logic_vector(4 downto 2));
end hczhvxhxbg;

architecture gr of hczhvxhxbg is
  
begin
  
end gr;

entity tyhzdbfkk is
  port (kkrupian : out integer_vector(1 downto 4); krfvrtc : in real_vector(2 downto 3); cv : inout bit_vector(0 to 4));
end tyhzdbfkk;

library ieee;
use ieee.std_logic_1164.all;

architecture qiywzwons of tyhzdbfkk is
  signal oigpfh : std_logic_vector(4 downto 2);
  signal ngwqrdtlt : std_logic_vector(4 downto 2);
  signal ofldvwylfq : std_logic_vector(4 downto 2);
begin
  pmf : entity work.hczhvxhxbg
    port map (uguccjzj => ofldvwylfq);
  jwidaubej : entity work.hczhvxhxbg
    port map (uguccjzj => ngwqrdtlt);
  opkuyk : entity work.hczhvxhxbg
    port map (uguccjzj => oigpfh);
  
  -- Single-driven assignments
  cv <= ('0', '1', '0', '0', '1');
  kkrupian <= kkrupian;
  
  -- Multi-driven assignments
  ofldvwylfq <= ofldvwylfq;
end qiywzwons;

entity vkx is
  port (tyzmhs : linkage time; ktresviq : out boolean);
end vkx;

library ieee;
use ieee.std_logic_1164.all;

architecture usagi of vkx is
  signal kmj : bit_vector(0 to 4);
  signal bb : real_vector(2 downto 3);
  signal t : integer_vector(1 downto 4);
  signal fkhzzt : bit_vector(0 to 4);
  signal p : real_vector(2 downto 3);
  signal szdmdtg : integer_vector(1 downto 4);
  signal vwzya : std_logic_vector(4 downto 2);
begin
  xuwijkag : entity work.hczhvxhxbg
    port map (uguccjzj => vwzya);
  f : entity work.tyhzdbfkk
    port map (kkrupian => szdmdtg, krfvrtc => p, cv => fkhzzt);
  ijhmlzpkvx : entity work.tyhzdbfkk
    port map (kkrupian => t, krfvrtc => bb, cv => kmj);
  
  -- Single-driven assignments
  bb <= p;
  p <= (others => 0.0);
  ktresviq <= ktresviq;
  
  -- Multi-driven assignments
  vwzya <= ('U', '-', 'L');
end usagi;

entity nacbw is
  port (wmw : linkage integer; aedpghzsfq : out real);
end nacbw;

architecture owpxl of nacbw is
  
begin
  -- Single-driven assignments
  aedpghzsfq <= 2_4.4;
end owpxl;



-- Seed after: 18293539652444292775,14641901754878719179
