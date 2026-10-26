-- Seed: 16463878031297811915,6562834012285671529

entity otzwp is
  port (nonpxd : buffer time; v : linkage time);
end otzwp;

architecture ycshxs of otzwp is
  
begin
  -- Single-driven assignments
  nonpxd <= nonpxd;
end ycshxs;

library ieee;
use ieee.std_logic_1164.all;

entity yomdldrrcl is
  port (s : in std_logic_vector(4 to 2); yoxxn : buffer real);
end yomdldrrcl;

architecture rnyzh of yomdldrrcl is
  signal gyk : time;
  signal zmngixx : time;
  signal yk : time;
  signal aa : time;
  signal ybkctmdipa : time;
  signal w : time;
begin
  uc : entity work.otzwp
    port map (nonpxd => w, v => ybkctmdipa);
  mhxyo : entity work.otzwp
    port map (nonpxd => aa, v => yk);
  bajfdzd : entity work.otzwp
    port map (nonpxd => zmngixx, v => gyk);
end rnyzh;

library ieee;
use ieee.std_logic_1164.all;

entity n is
  port (vgp : in std_logic_vector(0 to 4); lvci : inout integer; zpqof : in time; q : linkage std_logic_vector(2 to 1));
end n;

library ieee;
use ieee.std_logic_1164.all;

architecture mh of n is
  signal dkvywssn : real;
  signal zthvvqlhdf : std_logic_vector(4 to 2);
begin
  yckrmbern : entity work.yomdldrrcl
    port map (s => zthvvqlhdf, yoxxn => dkvywssn);
  
  -- Single-driven assignments
  lvci <= 2144;
  
  -- Multi-driven assignments
  zthvvqlhdf <= "";
end mh;



-- Seed after: 14953984571377489745,6562834012285671529
