-- Seed: 12767080310295198909,10463297573877745897

library ieee;
use ieee.std_logic_1164.all;

entity cc is
  port (bsiq : in real; uwxplyayn : out std_logic_vector(3 downto 1); tcddatvx : linkage std_logic);
end cc;

architecture djralwig of cc is
  
begin
  
end djralwig;

library ieee;
use ieee.std_logic_1164.all;

entity fgrfzcyoq is
  port (iegk : in time; vsex : linkage severity_level; wvs : in std_logic_vector(4 downto 1); qo : in std_logic_vector(0 to 2));
end fgrfzcyoq;

library ieee;
use ieee.std_logic_1164.all;

architecture bvdoxgyxy of fgrfzcyoq is
  signal hyl : std_logic_vector(3 downto 1);
  signal nlmx : std_logic;
  signal oxtmkonbw : std_logic_vector(3 downto 1);
  signal cqsdwvfrm : real;
  signal uilhw : std_logic;
  signal aacurbe : std_logic_vector(3 downto 1);
  signal iyiouoq : real;
begin
  fofkjrks : entity work.cc
    port map (bsiq => iyiouoq, uwxplyayn => aacurbe, tcddatvx => uilhw);
  dpfl : entity work.cc
    port map (bsiq => cqsdwvfrm, uwxplyayn => aacurbe, tcddatvx => uilhw);
  jd : entity work.cc
    port map (bsiq => cqsdwvfrm, uwxplyayn => oxtmkonbw, tcddatvx => nlmx);
  avkupvmwo : entity work.cc
    port map (bsiq => iyiouoq, uwxplyayn => hyl, tcddatvx => uilhw);
  
  -- Single-driven assignments
  iyiouoq <= iyiouoq;
  cqsdwvfrm <= iyiouoq;
  
  -- Multi-driven assignments
  uilhw <= '1';
  aacurbe <= ('-', 'Z', '1');
  aacurbe <= ('0', 'X', '0');
end bvdoxgyxy;

entity wfhj is
  port (sxc : linkage boolean; luxjgq : out integer; ygxunrp : out time; d : in integer);
end wfhj;

architecture oqdfpwf of wfhj is
  
begin
  
end oqdfpwf;

entity vfkyf is
  port (mdxzqcbu : in time);
end vfkyf;

library ieee;
use ieee.std_logic_1164.all;

architecture ezzg of vfkyf is
  signal tzpebej : std_logic_vector(0 to 2);
  signal oqnzibqy : std_logic_vector(4 downto 1);
  signal pt : severity_level;
  signal sww : time;
  signal q : integer;
  signal tyyqww : boolean;
begin
  i : entity work.wfhj
    port map (sxc => tyyqww, luxjgq => q, ygxunrp => sww, d => q);
  jr : entity work.fgrfzcyoq
    port map (iegk => sww, vsex => pt, wvs => oqnzibqy, qo => tzpebej);
  
  -- Multi-driven assignments
  oqnzibqy <= oqnzibqy;
  tzpebej <= tzpebej;
end ezzg;



-- Seed after: 12063921043137064616,10463297573877745897
