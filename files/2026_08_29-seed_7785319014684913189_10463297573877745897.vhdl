-- Seed: 7785319014684913189,10463297573877745897

entity itz is
  port (wzsqqffko : inout integer; hnpe : linkage bit_vector(2 downto 4); yufpvtbtsu : buffer integer);
end itz;

architecture mvldgpnuel of itz is
  
begin
  -- Single-driven assignments
  wzsqqffko <= 1;
end mvldgpnuel;

entity qie is
  port (brghntl : buffer integer; ri : in integer);
end qie;

architecture hgdwa of qie is
  signal phgzw : bit_vector(2 downto 4);
  signal xuhwyi : integer;
  signal whspqme : integer;
  signal kcejq : bit_vector(2 downto 4);
  signal yxdfzet : integer;
  signal sz : integer;
  signal llfnr : bit_vector(2 downto 4);
  signal gd : integer;
  signal vkmjfwp : integer;
  signal niy : bit_vector(2 downto 4);
  signal pmuzcuqnfv : integer;
begin
  kwnvcw : entity work.itz
    port map (wzsqqffko => pmuzcuqnfv, hnpe => niy, yufpvtbtsu => vkmjfwp);
  ayekbo : entity work.itz
    port map (wzsqqffko => gd, hnpe => llfnr, yufpvtbtsu => sz);
  qaqixdx : entity work.itz
    port map (wzsqqffko => yxdfzet, hnpe => kcejq, yufpvtbtsu => whspqme);
  wou : entity work.itz
    port map (wzsqqffko => xuhwyi, hnpe => phgzw, yufpvtbtsu => brghntl);
end hgdwa;

library ieee;
use ieee.std_logic_1164.all;

entity zu is
  port (wxgoxaduy : inout std_logic; ytcjorv : buffer integer; jwo : inout time);
end zu;

architecture vydj of zu is
  signal h : integer;
  signal q : bit_vector(2 downto 4);
  signal mqhnzz : integer;
  signal hjl : bit_vector(2 downto 4);
  signal hofnkgv : integer;
  signal fpqo : integer;
  signal eaz : bit_vector(2 downto 4);
  signal faezhcgh : integer;
begin
  youdcr : entity work.itz
    port map (wzsqqffko => faezhcgh, hnpe => eaz, yufpvtbtsu => fpqo);
  lglbjg : entity work.itz
    port map (wzsqqffko => hofnkgv, hnpe => hjl, yufpvtbtsu => mqhnzz);
  zfudv : entity work.itz
    port map (wzsqqffko => ytcjorv, hnpe => q, yufpvtbtsu => h);
  
  -- Single-driven assignments
  jwo <= jwo;
  
  -- Multi-driven assignments
  wxgoxaduy <= wxgoxaduy;
  wxgoxaduy <= 'W';
end vydj;



-- Seed after: 7542928159596891041,10463297573877745897
