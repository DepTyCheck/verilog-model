-- Seed: 13000079041322680340,3687118713772291287

entity hpugrlvw is
  port (rzpeyaj : in time; flf : inout severity_level);
end hpugrlvw;

architecture lb of hpugrlvw is
  
begin
  -- Single-driven assignments
  flf <= ERROR;
end lb;

entity mq is
  port (dgsnupxejz : out integer; bh : inout real);
end mq;

architecture wokhjkjtfw of mq is
  signal sxfcmatwx : severity_level;
  signal nmrcgqq : time;
begin
  ow : entity work.hpugrlvw
    port map (rzpeyaj => nmrcgqq, flf => sxfcmatwx);
  
  -- Single-driven assignments
  nmrcgqq <= 2 min;
end wokhjkjtfw;

library ieee;
use ieee.std_logic_1164.all;

entity wdpcankd is
  port (izm : linkage integer; mxq : linkage integer_vector(2 to 3); yjcrwxlbrm : inout std_logic_vector(2 downto 3); c : in std_logic_vector(0 to 0));
end wdpcankd;

architecture ji of wdpcankd is
  
begin
  -- Multi-driven assignments
  yjcrwxlbrm <= "";
end ji;

entity uiyuelpspm is
  port (fqj : in string(2 downto 4); ggkcjrms : in bit);
end uiyuelpspm;

library ieee;
use ieee.std_logic_1164.all;

architecture oldyxe of uiyuelpspm is
  signal xawa : severity_level;
  signal mbkawbwkrp : std_logic_vector(0 to 0);
  signal twl : integer_vector(2 to 3);
  signal amewqd : integer;
  signal mnnbssst : severity_level;
  signal w : time;
  signal mmtwo : std_logic_vector(0 to 0);
  signal denxs : std_logic_vector(2 downto 3);
  signal aealntjmzh : integer_vector(2 to 3);
  signal wb : integer;
begin
  b : entity work.wdpcankd
    port map (izm => wb, mxq => aealntjmzh, yjcrwxlbrm => denxs, c => mmtwo);
  pg : entity work.hpugrlvw
    port map (rzpeyaj => w, flf => mnnbssst);
  zx : entity work.wdpcankd
    port map (izm => amewqd, mxq => twl, yjcrwxlbrm => denxs, c => mbkawbwkrp);
  f : entity work.hpugrlvw
    port map (rzpeyaj => w, flf => xawa);
  
  -- Single-driven assignments
  w <= 2 min;
  
  -- Multi-driven assignments
  mbkawbwkrp <= (others => 'Z');
end oldyxe;



-- Seed after: 18318980517876990295,3687118713772291287
