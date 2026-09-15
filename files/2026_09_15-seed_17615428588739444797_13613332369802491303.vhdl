-- Seed: 17615428588739444797,13613332369802491303

library ieee;
use ieee.std_logic_1164.all;

entity hyyhjf is
  port (eznlzb : inout integer_vector(2 downto 4); mcxsbt : out real; culpdmtcjc : inout std_logic_vector(0 downto 3));
end hyyhjf;

architecture htqyyko of hyyhjf is
  
begin
  -- Single-driven assignments
  mcxsbt <= 8#2_3_7.1_6#;
  eznlzb <= (others => 0);
end htqyyko;

entity uys is
  port (jqfiepl : inout time);
end uys;

library ieee;
use ieee.std_logic_1164.all;

architecture dosvpdfew of uys is
  signal haobana : std_logic_vector(0 downto 3);
  signal hptp : real;
  signal mrssfyifg : integer_vector(2 downto 4);
begin
  bfuwf : entity work.hyyhjf
    port map (eznlzb => mrssfyifg, mcxsbt => hptp, culpdmtcjc => haobana);
  
  -- Single-driven assignments
  jqfiepl <= jqfiepl;
  
  -- Multi-driven assignments
  haobana <= haobana;
  haobana <= haobana;
  haobana <= "";
end dosvpdfew;



-- Seed after: 5262908209193470879,13613332369802491303
