-- Seed: 16307512114145593615,2338584220606314193

library ieee;
use ieee.std_logic_1164.all;

entity pdn is
  port (crtftniak : in real_vector(3 to 2); cxja : linkage boolean; nknbvm : out std_logic_vector(0 downto 1));
end pdn;

architecture w of pdn is
  
begin
  -- Multi-driven assignments
  nknbvm <= (others => '0');
  nknbvm <= "";
  nknbvm <= "";
end w;

entity dwjbacue is
  port (fmtezcflcr : in real_vector(2 to 2));
end dwjbacue;

library ieee;
use ieee.std_logic_1164.all;

architecture cdxlzdqp of dwjbacue is
  signal yjzoz : std_logic_vector(0 downto 1);
  signal nguo : boolean;
  signal iwkmfmaug : real_vector(3 to 2);
  signal vdutrsiqi : boolean;
  signal woob : real_vector(3 to 2);
  signal owdmtbz : std_logic_vector(0 downto 1);
  signal zppgsagq : boolean;
  signal wl : real_vector(3 to 2);
begin
  vcuozwqx : entity work.pdn
    port map (crtftniak => wl, cxja => zppgsagq, nknbvm => owdmtbz);
  jrozbgn : entity work.pdn
    port map (crtftniak => woob, cxja => vdutrsiqi, nknbvm => owdmtbz);
  uanwa : entity work.pdn
    port map (crtftniak => iwkmfmaug, cxja => nguo, nknbvm => yjzoz);
  
  -- Single-driven assignments
  wl <= (others => 0.0);
  woob <= wl;
  iwkmfmaug <= wl;
  
  -- Multi-driven assignments
  yjzoz <= owdmtbz;
  yjzoz <= owdmtbz;
  yjzoz <= owdmtbz;
  owdmtbz <= (others => '0');
end cdxlzdqp;

library ieee;
use ieee.std_logic_1164.all;

entity y is
  port (qmlljmxsf : in time; ddzei : in std_logic_vector(2 downto 3); blggs : out integer);
end y;

library ieee;
use ieee.std_logic_1164.all;

architecture lwouxk of y is
  signal aulrpe : std_logic_vector(0 downto 1);
  signal ejf : boolean;
  signal cmzgout : real_vector(3 to 2);
begin
  r : entity work.pdn
    port map (crtftniak => cmzgout, cxja => ejf, nknbvm => aulrpe);
  
  -- Single-driven assignments
  blggs <= blggs;
  cmzgout <= (others => 0.0);
  
  -- Multi-driven assignments
  aulrpe <= aulrpe;
  aulrpe <= (others => '0');
  aulrpe <= "";
  aulrpe <= ddzei;
end lwouxk;



-- Seed after: 16240638548550959363,2338584220606314193
