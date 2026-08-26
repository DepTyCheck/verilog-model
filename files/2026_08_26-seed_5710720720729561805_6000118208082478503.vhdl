-- Seed: 5710720720729561805,6000118208082478503

library ieee;
use ieee.std_logic_1164.all;

entity tcpkwv is
  port (z : linkage real_vector(0 downto 0); jnqe : inout std_logic; kznryy : linkage integer; kzwqdq : buffer time);
end tcpkwv;

architecture vzqcb of tcpkwv is
  
begin
  
end vzqcb;

entity nkervb is
  port (z : linkage real);
end nkervb;

library ieee;
use ieee.std_logic_1164.all;

architecture uz of nkervb is
  signal yogsggq : time;
  signal jw : integer;
  signal os : std_logic;
  signal rehwmtf : real_vector(0 downto 0);
  signal mmackf : time;
  signal v : integer;
  signal smtivzlfoj : std_logic;
  signal vdudiqt : real_vector(0 downto 0);
begin
  ctowp : entity work.tcpkwv
    port map (z => vdudiqt, jnqe => smtivzlfoj, kznryy => v, kzwqdq => mmackf);
  xyawyzr : entity work.tcpkwv
    port map (z => rehwmtf, jnqe => os, kznryy => jw, kzwqdq => yogsggq);
  
  -- Multi-driven assignments
  smtivzlfoj <= 'Z';
  os <= 'X';
  os <= smtivzlfoj;
  smtivzlfoj <= 'X';
end uz;

entity xwmdrd is
  port (fcgfmq : buffer severity_level; hrerwtxgu : in integer; zcvj : out string(5 downto 1); vhelkupz : linkage severity_level);
end xwmdrd;

library ieee;
use ieee.std_logic_1164.all;

architecture fur of xwmdrd is
  signal fqfvni : time;
  signal qripbc : integer;
  signal immq : std_logic;
  signal tsvgw : real_vector(0 downto 0);
begin
  hc : entity work.tcpkwv
    port map (z => tsvgw, jnqe => immq, kznryy => qripbc, kzwqdq => fqfvni);
  
  -- Single-driven assignments
  zcvj <= zcvj;
  fcgfmq <= fcgfmq;
  
  -- Multi-driven assignments
  immq <= 'U';
  immq <= '1';
  immq <= '1';
end fur;



-- Seed after: 732182873898920500,6000118208082478503
