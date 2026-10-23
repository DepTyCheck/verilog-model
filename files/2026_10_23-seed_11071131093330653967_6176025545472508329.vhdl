-- Seed: 11071131093330653967,6176025545472508329

library ieee;
use ieee.std_logic_1164.all;

entity qnmineew is
  port (yazindk : in std_logic_vector(3 downto 2); anm : in real_vector(3 to 1); lnzzdebuf : buffer integer);
end qnmineew;

architecture ivyqcpl of qnmineew is
  
begin
  -- Single-driven assignments
  lnzzdebuf <= 0;
end ivyqcpl;

library ieee;
use ieee.std_logic_1164.all;

entity qdwtrezd is
  port (w : out std_logic);
end qdwtrezd;

library ieee;
use ieee.std_logic_1164.all;

architecture m of qdwtrezd is
  signal iiwbcspz : integer;
  signal byruy : integer;
  signal picqfp : real_vector(3 to 1);
  signal jyvmqwt : integer;
  signal rqvia : real_vector(3 to 1);
  signal nawl : std_logic_vector(3 downto 2);
begin
  evdtds : entity work.qnmineew
    port map (yazindk => nawl, anm => rqvia, lnzzdebuf => jyvmqwt);
  ycllzpi : entity work.qnmineew
    port map (yazindk => nawl, anm => picqfp, lnzzdebuf => byruy);
  tv : entity work.qnmineew
    port map (yazindk => nawl, anm => rqvia, lnzzdebuf => iiwbcspz);
  
  -- Single-driven assignments
  picqfp <= picqfp;
  
  -- Multi-driven assignments
  w <= 'X';
  w <= w;
end m;

entity zwty is
  port (fqlnkkqx : linkage integer);
end zwty;

library ieee;
use ieee.std_logic_1164.all;

architecture cxobsfur of zwty is
  signal cftng : integer;
  signal alvak : real_vector(3 to 1);
  signal dgkuoddlv : integer;
  signal lghgynp : real_vector(3 to 1);
  signal yudw : std_logic_vector(3 downto 2);
begin
  heha : entity work.qnmineew
    port map (yazindk => yudw, anm => lghgynp, lnzzdebuf => dgkuoddlv);
  ce : entity work.qnmineew
    port map (yazindk => yudw, anm => alvak, lnzzdebuf => cftng);
  
  -- Single-driven assignments
  lghgynp <= (others => 0.0);
  alvak <= lghgynp;
  
  -- Multi-driven assignments
  yudw <= ('0', 'L');
  yudw <= yudw;
end cxobsfur;

entity wrncjlcbdy is
  port (ddtyjtjjy : linkage boolean);
end wrncjlcbdy;

library ieee;
use ieee.std_logic_1164.all;

architecture wmvpr of wrncjlcbdy is
  signal grszz : std_logic;
  signal xdiyeolku : integer;
  signal fds : real_vector(3 to 1);
  signal latficsp : integer;
  signal ddibnl : real_vector(3 to 1);
  signal xy : std_logic_vector(3 downto 2);
begin
  e : entity work.qnmineew
    port map (yazindk => xy, anm => ddibnl, lnzzdebuf => latficsp);
  sdujdkerx : entity work.qnmineew
    port map (yazindk => xy, anm => fds, lnzzdebuf => xdiyeolku);
  wvyjryfvq : entity work.qdwtrezd
    port map (w => grszz);
  
  -- Single-driven assignments
  ddibnl <= (others => 0.0);
  fds <= ddibnl;
  
  -- Multi-driven assignments
  grszz <= '-';
  grszz <= grszz;
  xy <= xy;
  xy <= xy;
end wmvpr;



-- Seed after: 618690713885433813,6176025545472508329
