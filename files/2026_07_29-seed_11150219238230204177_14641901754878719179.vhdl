-- Seed: 11150219238230204177,14641901754878719179

library ieee;
use ieee.std_logic_1164.all;

entity hyxlxgr is
  port (wtgptzc : inout bit_vector(0 downto 1); krs : buffer std_logic_vector(0 downto 4));
end hyxlxgr;

architecture kmlobn of hyxlxgr is
  
begin
  -- Single-driven assignments
  wtgptzc <= wtgptzc;
  
  -- Multi-driven assignments
  krs <= (others => '0');
  krs <= krs;
  krs <= (others => '0');
  krs <= krs;
end kmlobn;

library ieee;
use ieee.std_logic_1164.all;

entity y is
  port (qd : out boolean; bwanhl : linkage std_logic; sut : linkage time_vector(2 to 2); lkuz : inout time_vector(1 to 1));
end y;

library ieee;
use ieee.std_logic_1164.all;

architecture u of y is
  signal yijhxs : bit_vector(0 downto 1);
  signal nbbjrz : std_logic_vector(0 downto 4);
  signal kavpfmdl : bit_vector(0 downto 1);
begin
  dije : entity work.hyxlxgr
    port map (wtgptzc => kavpfmdl, krs => nbbjrz);
  om : entity work.hyxlxgr
    port map (wtgptzc => yijhxs, krs => nbbjrz);
  
  -- Single-driven assignments
  lkuz <= lkuz;
  qd <= qd;
  
  -- Multi-driven assignments
  nbbjrz <= nbbjrz;
  nbbjrz <= "";
  nbbjrz <= nbbjrz;
  nbbjrz <= (others => '0');
end u;

entity ctnpjorrg is
  port (kocxiqu : buffer integer; yfnbs : out time; sgkgzbc : inout integer);
end ctnpjorrg;

library ieee;
use ieee.std_logic_1164.all;

architecture bdvxy of ctnpjorrg is
  signal j : time_vector(1 to 1);
  signal r : time_vector(2 to 2);
  signal xclxwxil : std_logic;
  signal bzhyr : boolean;
begin
  glrltmuvfu : entity work.y
    port map (qd => bzhyr, bwanhl => xclxwxil, sut => r, lkuz => j);
  
  -- Single-driven assignments
  yfnbs <= 2 ps;
  kocxiqu <= 8#2_4#;
  sgkgzbc <= 3;
  
  -- Multi-driven assignments
  xclxwxil <= xclxwxil;
end bdvxy;



-- Seed after: 14674595664175544601,14641901754878719179
