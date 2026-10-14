-- Seed: 9512732376147467604,2287986438313874379

library ieee;
use ieee.std_logic_1164.all;

entity etxki is
  port (huozg : in std_logic_vector(0 to 1); zotm : in time; k : linkage integer; xhlw : linkage real);
end etxki;

architecture imk of etxki is
  
begin
  
end imk;

library ieee;
use ieee.std_logic_1164.all;

entity qrx is
  port (dfqthgum : buffer bit; cfffapl : out std_logic_vector(1 downto 3));
end qrx;

library ieee;
use ieee.std_logic_1164.all;

architecture lmxll of qrx is
  signal mhrbdqhkb : real;
  signal rlt : integer;
  signal jnxtp : time;
  signal twgpcdff : std_logic_vector(0 to 1);
  signal hvkvbxg : real;
  signal i : integer;
  signal hienpqzct : real;
  signal hvsujl : integer;
  signal qosywvaj : time;
  signal ajfnm : std_logic_vector(0 to 1);
  signal mpkcik : real;
  signal evdtmuyv : integer;
  signal ynnrefuk : time;
  signal wmfaafajc : std_logic_vector(0 to 1);
begin
  yts : entity work.etxki
    port map (huozg => wmfaafajc, zotm => ynnrefuk, k => evdtmuyv, xhlw => mpkcik);
  pxhiw : entity work.etxki
    port map (huozg => ajfnm, zotm => qosywvaj, k => hvsujl, xhlw => hienpqzct);
  rpbbmzu : entity work.etxki
    port map (huozg => ajfnm, zotm => qosywvaj, k => i, xhlw => hvkvbxg);
  vjj : entity work.etxki
    port map (huozg => twgpcdff, zotm => jnxtp, k => rlt, xhlw => mhrbdqhkb);
  
  -- Single-driven assignments
  jnxtp <= 3012.34302 ps;
  qosywvaj <= 40023.4 ps;
  dfqthgum <= '0';
  ynnrefuk <= 16#7.6681# ns;
  
  -- Multi-driven assignments
  ajfnm <= "0Z";
  cfffapl <= "";
end lmxll;



-- Seed after: 8063313020085735000,2287986438313874379
