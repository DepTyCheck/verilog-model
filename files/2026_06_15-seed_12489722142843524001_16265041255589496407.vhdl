-- Seed: 12489722142843524001,16265041255589496407

library ieee;
use ieee.std_logic_1164.all;

entity enlefsjvw is
  port (hndyrhp : inout std_logic_vector(2 downto 3); zfdu : in std_logic_vector(3 downto 3); k : linkage std_logic; vfmsjk : linkage real);
end enlefsjvw;



architecture qjyqn of enlefsjvw is
  
begin
  
end qjyqn;



entity cc is
  port (llasizycqp : out time_vector(2 downto 4); bg : buffer boolean_vector(1 downto 2); lyjzywm : out real);
end cc;

library ieee;
use ieee.std_logic_1164.all;

architecture ksy of cc is
  signal jbqdys : std_logic_vector(3 downto 3);
  signal bhrskbku : std_logic_vector(2 downto 3);
  signal i : real;
  signal mgxpxmfo : std_logic;
  signal grubrdlpd : std_logic_vector(3 downto 3);
  signal ybpdtxam : std_logic_vector(2 downto 3);
begin
  vfjiqujy : entity work.enlefsjvw
    port map (hndyrhp => ybpdtxam, zfdu => grubrdlpd, k => mgxpxmfo, vfmsjk => i);
  tweso : entity work.enlefsjvw
    port map (hndyrhp => bhrskbku, zfdu => jbqdys, k => mgxpxmfo, vfmsjk => lyjzywm);
end ksy;

library ieee;
use ieee.std_logic_1164.all;

entity rmsk is
  port (afjcigk : out integer; hzlff : out std_logic; lpqoy : inout integer; ox : linkage boolean_vector(1 downto 0));
end rmsk;

library ieee;
use ieee.std_logic_1164.all;

architecture tnwte of rmsk is
  signal lbptp : real;
  signal m : std_logic;
  signal brijkws : real;
  signal wszshhz : std_logic_vector(3 downto 3);
  signal wwzget : std_logic_vector(2 downto 3);
begin
  kiedeztjps : entity work.enlefsjvw
    port map (hndyrhp => wwzget, zfdu => wszshhz, k => hzlff, vfmsjk => brijkws);
  de : entity work.enlefsjvw
    port map (hndyrhp => wwzget, zfdu => wszshhz, k => m, vfmsjk => lbptp);
end tnwte;

library ieee;
use ieee.std_logic_1164.all;

entity bsc is
  port (ibluuo : out std_logic; v : buffer time; ig : inout std_logic_vector(4 downto 4); dai : in std_logic);
end bsc;

library ieee;
use ieee.std_logic_1164.all;

architecture pmwzkvm of bsc is
  signal xudrw : std_logic_vector(2 downto 3);
  signal zxjrtr : real;
  signal jgqspqj : std_logic;
  signal uhcv : std_logic_vector(3 downto 3);
  signal qhakpgc : real;
  signal th : std_logic_vector(2 downto 3);
begin
  vovkapf : entity work.enlefsjvw
    port map (hndyrhp => th, zfdu => ig, k => dai, vfmsjk => qhakpgc);
  qpdjcjerir : entity work.enlefsjvw
    port map (hndyrhp => th, zfdu => uhcv, k => jgqspqj, vfmsjk => zxjrtr);
  flkh : entity work.enlefsjvw
    port map (hndyrhp => xudrw, zfdu => ig, k => jgqspqj, vfmsjk => qhakpgc);
end pmwzkvm;



-- Seed after: 6510478078769068898,16265041255589496407
