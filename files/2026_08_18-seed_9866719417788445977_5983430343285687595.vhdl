-- Seed: 9866719417788445977,5983430343285687595

library ieee;
use ieee.std_logic_1164.all;

entity yp is
  port (bepsyoze : out std_logic_vector(3 to 4); xuhwwatnsc : in time; vlivebcqq : inout std_logic_vector(3 downto 0));
end yp;

architecture wey of yp is
  
begin
  
end wey;

library ieee;
use ieee.std_logic_1164.all;

entity o is
  port (vwbz : in time; y : in integer; mtxtbi : linkage real; aqyhwzjv : linkage std_logic_vector(1 downto 2));
end o;

library ieee;
use ieee.std_logic_1164.all;

architecture p of o is
  signal yhgsvmll : std_logic_vector(3 downto 0);
  signal fkrhzidh : time;
  signal csmcbcwyu : time;
  signal qtohkjwshf : time;
  signal fxfy : std_logic_vector(3 to 4);
  signal pbabu : std_logic_vector(3 downto 0);
  signal bofbnuyh : std_logic_vector(3 to 4);
begin
  ugvpmjj : entity work.yp
    port map (bepsyoze => bofbnuyh, xuhwwatnsc => vwbz, vlivebcqq => pbabu);
  leewtbl : entity work.yp
    port map (bepsyoze => fxfy, xuhwwatnsc => qtohkjwshf, vlivebcqq => pbabu);
  bqqqrt : entity work.yp
    port map (bepsyoze => bofbnuyh, xuhwwatnsc => csmcbcwyu, vlivebcqq => pbabu);
  t : entity work.yp
    port map (bepsyoze => bofbnuyh, xuhwwatnsc => fkrhzidh, vlivebcqq => yhgsvmll);
  
  -- Single-driven assignments
  qtohkjwshf <= vwbz;
  fkrhzidh <= vwbz;
  csmcbcwyu <= 2#1_0_1.1# ps;
  
  -- Multi-driven assignments
  bofbnuyh <= bofbnuyh;
  fxfy <= bofbnuyh;
  bofbnuyh <= ('Z', 'L');
end p;



-- Seed after: 5851107611369567748,5983430343285687595
