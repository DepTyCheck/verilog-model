-- Seed: 16402462962006320042,9099465110516446487

entity wfxzujp is
  port (dw : in integer; wow : buffer real; ln : buffer real; fgefnfm : buffer real);
end wfxzujp;

architecture bntwz of wfxzujp is
  
begin
  -- Single-driven assignments
  wow <= 16#9_D_1_A_D.E_2_F_2#;
end bntwz;

entity s is
  port (oyfcto : inout real; dmy : buffer time_vector(0 downto 3); hymtehfnrj : buffer time_vector(0 downto 1));
end s;

architecture othpih of s is
  signal jfwrotnj : real;
  signal m : real;
  signal mmyuyvig : real;
  signal w : real;
  signal ckrrbxcu : real;
  signal picxcbecd : real;
  signal d : real;
  signal pwd : real;
  signal dgoxt : real;
  signal ewqnhg : real;
  signal fjdadz : real;
  signal ltqnpvg : integer;
begin
  lxicnl : entity work.wfxzujp
    port map (dw => ltqnpvg, wow => fjdadz, ln => ewqnhg, fgefnfm => dgoxt);
  jrdczbe : entity work.wfxzujp
    port map (dw => ltqnpvg, wow => pwd, ln => d, fgefnfm => picxcbecd);
  ftxqh : entity work.wfxzujp
    port map (dw => ltqnpvg, wow => ckrrbxcu, ln => w, fgefnfm => mmyuyvig);
  sxxnckyf : entity work.wfxzujp
    port map (dw => ltqnpvg, wow => oyfcto, ln => m, fgefnfm => jfwrotnj);
end othpih;

library ieee;
use ieee.std_logic_1164.all;

entity rndv is
  port (etylwg : inout time; soxkbipo : out std_logic_vector(3 to 4));
end rndv;

architecture clrmoveqv of rndv is
  signal yvberi : time_vector(0 downto 1);
  signal wsl : time_vector(0 downto 3);
  signal q : real;
  signal eydawpfmw : time_vector(0 downto 1);
  signal mb : time_vector(0 downto 3);
  signal h : real;
  signal dwmjtyh : time_vector(0 downto 1);
  signal evrmasqy : time_vector(0 downto 3);
  signal putrgzun : real;
begin
  jdchr : entity work.s
    port map (oyfcto => putrgzun, dmy => evrmasqy, hymtehfnrj => dwmjtyh);
  uorzxeojv : entity work.s
    port map (oyfcto => h, dmy => mb, hymtehfnrj => eydawpfmw);
  htbuwpdss : entity work.s
    port map (oyfcto => q, dmy => wsl, hymtehfnrj => yvberi);
  
  -- Single-driven assignments
  etylwg <= etylwg;
  
  -- Multi-driven assignments
  soxkbipo <= "LH";
  soxkbipo <= "XU";
  soxkbipo <= "L0";
end clrmoveqv;



-- Seed after: 1574047551524135352,9099465110516446487
