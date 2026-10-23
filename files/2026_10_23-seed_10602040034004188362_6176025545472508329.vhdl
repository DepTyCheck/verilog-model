-- Seed: 10602040034004188362,6176025545472508329

library ieee;
use ieee.std_logic_1164.all;

entity deujycdctz is
  port (qq : linkage std_logic_vector(3 downto 3); rlniwtdvrk : in real; cdbkmos : out real);
end deujycdctz;

architecture wp of deujycdctz is
  
begin
  
end wp;

library ieee;
use ieee.std_logic_1164.all;

entity uenowm is
  port (fqleegchny : buffer std_logic_vector(0 to 3); nwp : inout time; bdm : linkage real);
end uenowm;

library ieee;
use ieee.std_logic_1164.all;

architecture amayuzvpj of uenowm is
  signal gbrkumyg : std_logic_vector(3 downto 3);
  signal ep : real;
  signal xlrco : std_logic_vector(3 downto 3);
  signal mw : real;
  signal dklldizl : real;
  signal f : real;
  signal hn : std_logic_vector(3 downto 3);
begin
  zzkz : entity work.deujycdctz
    port map (qq => hn, rlniwtdvrk => f, cdbkmos => dklldizl);
  m : entity work.deujycdctz
    port map (qq => hn, rlniwtdvrk => mw, cdbkmos => f);
  kueyouhhmk : entity work.deujycdctz
    port map (qq => xlrco, rlniwtdvrk => mw, cdbkmos => ep);
  bvoksmsaj : entity work.deujycdctz
    port map (qq => gbrkumyg, rlniwtdvrk => ep, cdbkmos => mw);
  
  -- Single-driven assignments
  nwp <= nwp;
  
  -- Multi-driven assignments
  fqleegchny <= ('-', '-', 'W', 'Z');
end amayuzvpj;

library ieee;
use ieee.std_logic_1164.all;

entity ivhug is
  port (jtx : inout std_logic_vector(4 to 4); gjeig : in time; uymrp : in bit);
end ivhug;

library ieee;
use ieee.std_logic_1164.all;

architecture gosfaujem of ivhug is
  signal szibvhgk : real;
  signal awt : real;
  signal oeyst : time;
  signal iqm : real;
  signal plqxjdqk : time;
  signal kzgreizx : std_logic_vector(0 to 3);
begin
  wmncj : entity work.uenowm
    port map (fqleegchny => kzgreizx, nwp => plqxjdqk, bdm => iqm);
  hxltob : entity work.uenowm
    port map (fqleegchny => kzgreizx, nwp => oeyst, bdm => awt);
  aai : entity work.deujycdctz
    port map (qq => jtx, rlniwtdvrk => szibvhgk, cdbkmos => szibvhgk);
  
  -- Multi-driven assignments
  kzgreizx <= "XZWW";
  jtx <= (others => 'L');
  jtx <= (others => '1');
end gosfaujem;



-- Seed after: 13128659403206387865,6176025545472508329
