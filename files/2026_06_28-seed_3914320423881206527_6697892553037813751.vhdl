-- Seed: 3914320423881206527,6697892553037813751

library ieee;
use ieee.std_logic_1164.all;

entity ilpou is
  port (sqbfpi : buffer time_vector(2 to 4); aymcylzyxd : out boolean; dn : out std_logic; tqrm : in integer);
end ilpou;

architecture fpztnsdyl of ilpou is
  
begin
  -- Multi-driven assignments
  dn <= 'Z';
  dn <= 'U';
  dn <= '0';
end fpztnsdyl;

entity vnvuwzh is
  port (bqcsdjn : in bit_vector(1 to 4); vqdmwvewwd : out time; h : linkage real; hspuc : inout time);
end vnvuwzh;

library ieee;
use ieee.std_logic_1164.all;

architecture kgit of vnvuwzh is
  signal k : integer;
  signal ezigcrulw : boolean;
  signal mcph : time_vector(2 to 4);
  signal vrqs : std_logic;
  signal unxnctxxep : boolean;
  signal vdysddi : time_vector(2 to 4);
  signal qtwubkwe : integer;
  signal rhvvdxh : std_logic;
  signal nracmmja : boolean;
  signal wuwfc : time_vector(2 to 4);
  signal ugqnrlhauy : integer;
  signal kuj : std_logic;
  signal gtsmrqye : boolean;
  signal zigr : time_vector(2 to 4);
begin
  ddwys : entity work.ilpou
    port map (sqbfpi => zigr, aymcylzyxd => gtsmrqye, dn => kuj, tqrm => ugqnrlhauy);
  eoz : entity work.ilpou
    port map (sqbfpi => wuwfc, aymcylzyxd => nracmmja, dn => rhvvdxh, tqrm => qtwubkwe);
  pwdx : entity work.ilpou
    port map (sqbfpi => vdysddi, aymcylzyxd => unxnctxxep, dn => vrqs, tqrm => ugqnrlhauy);
  unaguyyd : entity work.ilpou
    port map (sqbfpi => mcph, aymcylzyxd => ezigcrulw, dn => kuj, tqrm => k);
  
  -- Single-driven assignments
  ugqnrlhauy <= 2#111#;
  k <= 3211;
  qtwubkwe <= 2#000#;
  vqdmwvewwd <= 2_4 us;
  hspuc <= 2_4 ns;
  
  -- Multi-driven assignments
  kuj <= 'H';
  kuj <= 'X';
  vrqs <= '-';
  rhvvdxh <= 'H';
end kgit;



-- Seed after: 1058379498479483743,6697892553037813751
