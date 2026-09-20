-- Seed: 17119069302643769981,18037650846010261179

entity jdedc is
  port (kokt : out bit);
end jdedc;

architecture fgcz of jdedc is
  
begin
  -- Single-driven assignments
  kokt <= '1';
end fgcz;

entity wooptd is
  port (slhma : inout severity_level);
end wooptd;

architecture fnncxidzy of wooptd is
  signal zqasnajah : bit;
  signal h : bit;
  signal rffg : bit;
  signal haxpdyemjx : bit;
begin
  sxfusmutn : entity work.jdedc
    port map (kokt => haxpdyemjx);
  mzzfag : entity work.jdedc
    port map (kokt => rffg);
  didkgkymuq : entity work.jdedc
    port map (kokt => h);
  kcmytylzv : entity work.jdedc
    port map (kokt => zqasnajah);
  
  -- Single-driven assignments
  slhma <= ERROR;
end fnncxidzy;

library ieee;
use ieee.std_logic_1164.all;

entity wmqc is
  port (z : out std_logic_vector(0 to 0));
end wmqc;

architecture hqu of wmqc is
  signal fjej : bit;
  signal yas : bit;
  signal ap : bit;
  signal ylgrukx : severity_level;
begin
  aoufj : entity work.wooptd
    port map (slhma => ylgrukx);
  xd : entity work.jdedc
    port map (kokt => ap);
  ozdzgoep : entity work.jdedc
    port map (kokt => yas);
  cqauqfaug : entity work.jdedc
    port map (kokt => fjej);
  
  -- Multi-driven assignments
  z <= z;
end hqu;

library ieee;
use ieee.std_logic_1164.all;

entity yzga is
  port (yiwpnmm : in std_logic_vector(3 to 3); lzvmke : in bit_vector(2 to 0));
end yzga;

library ieee;
use ieee.std_logic_1164.all;

architecture itjgyjfw of yzga is
  signal lm : std_logic_vector(0 to 0);
  signal acccniv : std_logic_vector(0 to 0);
begin
  f : entity work.wmqc
    port map (z => acccniv);
  zljxeelrl : entity work.wmqc
    port map (z => lm);
  
  -- Multi-driven assignments
  lm <= "W";
  lm <= yiwpnmm;
end itjgyjfw;



-- Seed after: 994354666320705568,18037650846010261179
