-- Seed: 12707186840408898027,3687118713772291287

entity omaqqroqmw is
  port (roqjebq : linkage real_vector(3 downto 1); gzw : in time_vector(1 downto 1));
end omaqqroqmw;

architecture rikkbmwuv of omaqqroqmw is
  
begin
  
end rikkbmwuv;

entity gjaj is
  port (mtdykhm : inout severity_level; yifeeshawp : buffer bit; nwodlnjf : buffer time; piwhwep : linkage time);
end gjaj;

architecture n of gjaj is
  signal sis : real_vector(3 downto 1);
  signal uvyvsvyvr : real_vector(3 downto 1);
  signal worosrmi : time_vector(1 downto 1);
  signal jqc : real_vector(3 downto 1);
  signal u : time_vector(1 downto 1);
  signal ondnwswpa : real_vector(3 downto 1);
begin
  yhccsi : entity work.omaqqroqmw
    port map (roqjebq => ondnwswpa, gzw => u);
  xj : entity work.omaqqroqmw
    port map (roqjebq => jqc, gzw => worosrmi);
  kdtqao : entity work.omaqqroqmw
    port map (roqjebq => uvyvsvyvr, gzw => u);
  xspes : entity work.omaqqroqmw
    port map (roqjebq => sis, gzw => u);
  
  -- Single-driven assignments
  nwodlnjf <= 8#72455.7# ps;
  u <= (others => 0_1_2_2_3.4 ps);
  worosrmi <= (others => 0 ns);
  yifeeshawp <= '1';
  mtdykhm <= ERROR;
end n;

library ieee;
use ieee.std_logic_1164.all;

entity ndbnzss is
  port (dvi : inout std_logic_vector(1 to 4));
end ndbnzss;

architecture kchuecuv of ndbnzss is
  signal fxdcfcbi : time;
  signal isempu : time;
  signal l : bit;
  signal jbdyeeb : severity_level;
  signal syntpmdk : time;
  signal wm : time;
  signal skqnpshan : bit;
  signal crihlegsmb : severity_level;
begin
  gwlaw : entity work.gjaj
    port map (mtdykhm => crihlegsmb, yifeeshawp => skqnpshan, nwodlnjf => wm, piwhwep => syntpmdk);
  vbzpgwxqy : entity work.gjaj
    port map (mtdykhm => jbdyeeb, yifeeshawp => l, nwodlnjf => isempu, piwhwep => fxdcfcbi);
end kchuecuv;

entity lt is
  port (o : out real);
end lt;

library ieee;
use ieee.std_logic_1164.all;

architecture cchayqyri of lt is
  signal bwtcisnao : time;
  signal kwbia : time;
  signal xzuudo : bit;
  signal exhxii : severity_level;
  signal rq : std_logic_vector(1 to 4);
  signal yrv : time_vector(1 downto 1);
  signal tp : real_vector(3 downto 1);
begin
  opbidgud : entity work.omaqqroqmw
    port map (roqjebq => tp, gzw => yrv);
  kwf : entity work.ndbnzss
    port map (dvi => rq);
  rvdfvrma : entity work.gjaj
    port map (mtdykhm => exhxii, yifeeshawp => xzuudo, nwodlnjf => kwbia, piwhwep => bwtcisnao);
  
  -- Multi-driven assignments
  rq <= ('0', '0', '1', '0');
end cchayqyri;



-- Seed after: 9243188887611418978,3687118713772291287
