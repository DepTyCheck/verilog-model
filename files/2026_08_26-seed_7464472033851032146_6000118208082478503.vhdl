-- Seed: 7464472033851032146,6000118208082478503

entity urc is
  port (zmu : buffer integer; xfchmb : buffer severity_level; sbgi : inout string(2 to 3));
end urc;

architecture he of urc is
  
begin
  -- Single-driven assignments
  zmu <= zmu;
  xfchmb <= ERROR;
  sbgi <= "ds";
end he;

library ieee;
use ieee.std_logic_1164.all;

entity j is
  port (eka : inout integer; mglgi : out std_logic_vector(4 downto 3); nwcalewb : in boolean_vector(4 to 1));
end j;

architecture o of j is
  signal ydz : string(2 to 3);
  signal wqmyb : severity_level;
  signal neibsj : integer;
  signal lfmbacr : string(2 to 3);
  signal q : severity_level;
  signal kdyknbqwt : integer;
  signal t : string(2 to 3);
  signal gz : severity_level;
begin
  hiqr : entity work.urc
    port map (zmu => eka, xfchmb => gz, sbgi => t);
  dwo : entity work.urc
    port map (zmu => kdyknbqwt, xfchmb => q, sbgi => lfmbacr);
  ayc : entity work.urc
    port map (zmu => neibsj, xfchmb => wqmyb, sbgi => ydz);
end o;



-- Seed after: 11192486287162376840,6000118208082478503
