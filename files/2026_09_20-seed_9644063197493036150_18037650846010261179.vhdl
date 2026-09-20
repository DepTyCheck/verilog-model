-- Seed: 9644063197493036150,18037650846010261179

entity tmeb is
  port (gev : in time; fxcxptnxy : out bit_vector(1 to 1); fwwm : in bit; r : linkage severity_level);
end tmeb;

architecture xvclj of tmeb is
  
begin
  -- Single-driven assignments
  fxcxptnxy <= (others => '0');
end xvclj;

library ieee;
use ieee.std_logic_1164.all;

entity a is
  port (vlnowedpnq : linkage std_logic_vector(4 downto 4); cyicwcafw : linkage time);
end a;

architecture cfgyyrmg of a is
  signal cdx : severity_level;
  signal ymiaoawir : bit_vector(1 to 1);
  signal csiuzil : severity_level;
  signal otbtrjyzwk : bit_vector(1 to 1);
  signal qgul : severity_level;
  signal xp : bit;
  signal vnmhlv : bit_vector(1 to 1);
  signal udssmos : severity_level;
  signal v : bit;
  signal ciw : bit_vector(1 to 1);
  signal zfjgincgeh : time;
begin
  yhcbivgywl : entity work.tmeb
    port map (gev => zfjgincgeh, fxcxptnxy => ciw, fwwm => v, r => udssmos);
  szi : entity work.tmeb
    port map (gev => zfjgincgeh, fxcxptnxy => vnmhlv, fwwm => xp, r => qgul);
  erth : entity work.tmeb
    port map (gev => zfjgincgeh, fxcxptnxy => otbtrjyzwk, fwwm => v, r => csiuzil);
  smcpxuzc : entity work.tmeb
    port map (gev => zfjgincgeh, fxcxptnxy => ymiaoawir, fwwm => v, r => cdx);
  
  -- Single-driven assignments
  zfjgincgeh <= zfjgincgeh;
  xp <= xp;
  v <= v;
end cfgyyrmg;



-- Seed after: 15452299804051705483,18037650846010261179
