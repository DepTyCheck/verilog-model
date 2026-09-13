-- Seed: 643905520778423657,10754487200446211253

library ieee;
use ieee.std_logic_1164.all;

entity kprutsout is
  port (ooksfpdj : inout bit_vector(1 to 1); asumg : buffer std_logic_vector(4 to 1));
end kprutsout;

architecture bqb of kprutsout is
  
begin
  
end bqb;

library ieee;
use ieee.std_logic_1164.all;

entity aoe is
  port (bq : linkage std_logic; gol : in time; q : inout real_vector(4 to 0));
end aoe;

library ieee;
use ieee.std_logic_1164.all;

architecture fgafknvhc of aoe is
  signal upbtjsa : bit_vector(1 to 1);
  signal hxf : bit_vector(1 to 1);
  signal vpwszckq : bit_vector(1 to 1);
  signal epubecjy : std_logic_vector(4 to 1);
  signal x : bit_vector(1 to 1);
begin
  f : entity work.kprutsout
    port map (ooksfpdj => x, asumg => epubecjy);
  gu : entity work.kprutsout
    port map (ooksfpdj => vpwszckq, asumg => epubecjy);
  drbbtozlro : entity work.kprutsout
    port map (ooksfpdj => hxf, asumg => epubecjy);
  vedetd : entity work.kprutsout
    port map (ooksfpdj => upbtjsa, asumg => epubecjy);
  
  -- Single-driven assignments
  q <= (others => 0.0);
  
  -- Multi-driven assignments
  epubecjy <= (others => '0');
  epubecjy <= epubecjy;
  epubecjy <= epubecjy;
  epubecjy <= epubecjy;
end fgafknvhc;



-- Seed after: 8230506060336681893,10754487200446211253
