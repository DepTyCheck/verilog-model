-- Seed: 4909539719897085507,5983430343285687595

library ieee;
use ieee.std_logic_1164.all;

entity cenkdrryip is
  port (wzlty : in std_logic_vector(1 downto 2); mfatlncv : in integer; mkaadszmj : in boolean_vector(4 downto 1); b : in std_logic_vector(1 downto 0));
end cenkdrryip;

architecture nzueqzptx of cenkdrryip is
  
begin
  
end nzueqzptx;

library ieee;
use ieee.std_logic_1164.all;

entity xglqivs is
  port (cfds : inout std_logic; bdv : out std_logic_vector(2 to 3); ggvw : inout std_logic_vector(3 to 2));
end xglqivs;

library ieee;
use ieee.std_logic_1164.all;

architecture lkznmx of xglqivs is
  signal vbpgah : boolean_vector(4 downto 1);
  signal lhuhsoyyvn : integer;
  signal jkm : std_logic_vector(1 downto 2);
  signal olhqn : std_logic_vector(1 downto 2);
  signal nscjgjcvw : boolean_vector(4 downto 1);
  signal wzyrz : integer;
  signal czubryy : std_logic_vector(1 downto 2);
begin
  gvn : entity work.cenkdrryip
    port map (wzlty => czubryy, mfatlncv => wzyrz, mkaadszmj => nscjgjcvw, b => bdv);
  s : entity work.cenkdrryip
    port map (wzlty => olhqn, mfatlncv => wzyrz, mkaadszmj => nscjgjcvw, b => bdv);
  xgnkgafvcb : entity work.cenkdrryip
    port map (wzlty => jkm, mfatlncv => lhuhsoyyvn, mkaadszmj => vbpgah, b => bdv);
  
  -- Single-driven assignments
  wzyrz <= 24;
  lhuhsoyyvn <= 2#0_0_1_0#;
  nscjgjcvw <= nscjgjcvw;
  vbpgah <= (FALSE, FALSE, FALSE, TRUE);
  
  -- Multi-driven assignments
  cfds <= cfds;
  jkm <= jkm;
end lkznmx;



-- Seed after: 6539438635038213774,5983430343285687595
