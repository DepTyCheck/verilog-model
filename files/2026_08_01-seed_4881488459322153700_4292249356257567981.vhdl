-- Seed: 4881488459322153700,4292249356257567981

entity p is
  port (pmbjmi : in integer; e : inout time; vhm : out severity_level; c : out bit);
end p;

architecture uf of p is
  
begin
  -- Single-driven assignments
  c <= '0';
  vhm <= ERROR;
  e <= e;
end uf;

entity khuazdfoaj is
  port (zkqqyf : in integer; szbhtc : inout integer; iysotu : out severity_level);
end khuazdfoaj;

architecture ltzta of khuazdfoaj is
  signal uyktozjdhb : bit;
  signal him : severity_level;
  signal bmv : time;
  signal orugdrcqa : bit;
  signal ni : severity_level;
  signal gt : time;
begin
  nlaczghzbd : entity work.p
    port map (pmbjmi => szbhtc, e => gt, vhm => ni, c => orugdrcqa);
  cymmyzd : entity work.p
    port map (pmbjmi => szbhtc, e => bmv, vhm => him, c => uyktozjdhb);
  
  -- Single-driven assignments
  iysotu <= him;
  szbhtc <= szbhtc;
end ltzta;

library ieee;
use ieee.std_logic_1164.all;

entity biccfuardv is
  port (ccpzlnkci : buffer std_logic; agomq : inout std_logic_vector(0 downto 1));
end biccfuardv;

architecture ruy of biccfuardv is
  
begin
  -- Multi-driven assignments
  agomq <= (others => '0');
  agomq <= agomq;
end ruy;



-- Seed after: 13620954176551428946,4292249356257567981
