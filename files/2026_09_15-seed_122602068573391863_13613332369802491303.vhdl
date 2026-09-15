-- Seed: 122602068573391863,13613332369802491303

library ieee;
use ieee.std_logic_1164.all;

entity nah is
  port (trhdscu : linkage time; wjcmtpxvp : linkage std_logic_vector(1 downto 4); nmgrctue : in boolean; uotepqmc : out std_logic_vector(1 downto 3));
end nah;

architecture y of nah is
  
begin
  -- Multi-driven assignments
  uotepqmc <= "";
  uotepqmc <= "";
  uotepqmc <= uotepqmc;
end y;

entity kyzqydc is
  port (ocpvru : out boolean);
end kyzqydc;

library ieee;
use ieee.std_logic_1164.all;

architecture swam of kyzqydc is
  signal rnzdhbdf : std_logic_vector(1 downto 3);
  signal ia : time;
  signal ndngnt : std_logic_vector(1 downto 3);
  signal klmzfkfliy : std_logic_vector(1 downto 4);
  signal cokav : time;
  signal oiupiuwbl : std_logic_vector(1 downto 4);
  signal nab : time;
begin
  xg : entity work.nah
    port map (trhdscu => nab, wjcmtpxvp => oiupiuwbl, nmgrctue => ocpvru, uotepqmc => oiupiuwbl);
  csjpcnej : entity work.nah
    port map (trhdscu => cokav, wjcmtpxvp => klmzfkfliy, nmgrctue => ocpvru, uotepqmc => ndngnt);
  jsbumhth : entity work.nah
    port map (trhdscu => ia, wjcmtpxvp => oiupiuwbl, nmgrctue => ocpvru, uotepqmc => rnzdhbdf);
  
  -- Single-driven assignments
  ocpvru <= ocpvru;
  
  -- Multi-driven assignments
  oiupiuwbl <= "";
  oiupiuwbl <= "";
  oiupiuwbl <= "";
end swam;

entity dglt is
  port (vffkjkkz : linkage time; qgazwszmev : out integer);
end dglt;

library ieee;
use ieee.std_logic_1164.all;

architecture rdz of dglt is
  signal uwtmx : std_logic_vector(1 downto 4);
  signal fuudyzy : time;
  signal tmv : boolean;
  signal w : std_logic_vector(1 downto 3);
  signal jvsspawgch : time;
begin
  a : entity work.nah
    port map (trhdscu => jvsspawgch, wjcmtpxvp => w, nmgrctue => tmv, uotepqmc => w);
  waluwy : entity work.nah
    port map (trhdscu => fuudyzy, wjcmtpxvp => uwtmx, nmgrctue => tmv, uotepqmc => w);
  
  -- Single-driven assignments
  qgazwszmev <= 034;
  tmv <= TRUE;
  
  -- Multi-driven assignments
  w <= w;
  w <= "";
  w <= (others => '0');
  w <= "";
end rdz;

library ieee;
use ieee.std_logic_1164.all;

entity euk is
  port (fxnor : buffer std_logic_vector(0 to 2); kaegc : buffer integer; fzqr : inout character);
end euk;

architecture najnzzyiaz of euk is
  signal ophils : integer;
  signal bvut : time;
  signal fiywo : boolean;
begin
  nkbmtoopu : entity work.kyzqydc
    port map (ocpvru => fiywo);
  ejxyqrfjo : entity work.dglt
    port map (vffkjkkz => bvut, qgazwszmev => ophils);
  
  -- Single-driven assignments
  fzqr <= 'x';
  
  -- Multi-driven assignments
  fxnor <= fxnor;
end najnzzyiaz;



-- Seed after: 15290701576858801071,13613332369802491303
