-- Seed: 16013742257929290340,7311216359267151659

library ieee;
use ieee.std_logic_1164.all;

entity mqxmfwxpf is
  port (xgqy : out real; pcsql : buffer integer_vector(0 to 0); xaadtgqei : linkage std_logic);
end mqxmfwxpf;

architecture doxzbkedol of mqxmfwxpf is
  
begin
  
end doxzbkedol;

entity oc is
  port (gjoociwtr : linkage time);
end oc;

library ieee;
use ieee.std_logic_1164.all;

architecture swlav of oc is
  signal rgkrdtdbyf : integer_vector(0 to 0);
  signal nbbbxxl : real;
  signal oriw : integer_vector(0 to 0);
  signal ipypyngdn : real;
  signal excajh : std_logic;
  signal peqt : integer_vector(0 to 0);
  signal swtrf : real;
begin
  mznc : entity work.mqxmfwxpf
    port map (xgqy => swtrf, pcsql => peqt, xaadtgqei => excajh);
  ejksrgduwr : entity work.mqxmfwxpf
    port map (xgqy => ipypyngdn, pcsql => oriw, xaadtgqei => excajh);
  mrfk : entity work.mqxmfwxpf
    port map (xgqy => nbbbxxl, pcsql => rgkrdtdbyf, xaadtgqei => excajh);
  
  -- Multi-driven assignments
  excajh <= '-';
  excajh <= 'H';
  excajh <= excajh;
  excajh <= excajh;
end swlav;

library ieee;
use ieee.std_logic_1164.all;

entity uzqpix is
  port (etbr : out boolean_vector(1 to 3); fayjktdgco : out std_logic; lto : linkage real; gpibn : in real_vector(3 to 1));
end uzqpix;

library ieee;
use ieee.std_logic_1164.all;

architecture kslms of uzqpix is
  signal nhond : time;
  signal esvq : std_logic;
  signal ydwstwfxja : integer_vector(0 to 0);
  signal gpo : real;
  signal nhhwomlou : std_logic;
  signal vcsimdsn : integer_vector(0 to 0);
  signal bcnvtxyh : real;
begin
  cqxqya : entity work.mqxmfwxpf
    port map (xgqy => bcnvtxyh, pcsql => vcsimdsn, xaadtgqei => nhhwomlou);
  hptgxp : entity work.mqxmfwxpf
    port map (xgqy => gpo, pcsql => ydwstwfxja, xaadtgqei => esvq);
  uqjrjt : entity work.oc
    port map (gjoociwtr => nhond);
  
  -- Multi-driven assignments
  nhhwomlou <= fayjktdgco;
end kslms;

library ieee;
use ieee.std_logic_1164.all;

entity advfsnq is
  port (dltbefac : buffer std_logic; dnf : in std_logic_vector(0 downto 4); udxfjjf : in std_logic; vq : inout bit);
end advfsnq;

library ieee;
use ieee.std_logic_1164.all;

architecture us of advfsnq is
  signal wvkbgsh : real_vector(3 to 1);
  signal ihoro : real;
  signal gnwcchyboq : std_logic;
  signal ch : boolean_vector(1 to 3);
  signal oaqn : integer_vector(0 to 0);
  signal fknqonvnpd : real;
  signal osmoa : time;
begin
  rzffcabr : entity work.oc
    port map (gjoociwtr => osmoa);
  ahhspncn : entity work.mqxmfwxpf
    port map (xgqy => fknqonvnpd, pcsql => oaqn, xaadtgqei => dltbefac);
  os : entity work.uzqpix
    port map (etbr => ch, fayjktdgco => gnwcchyboq, lto => ihoro, gpibn => wvkbgsh);
  
  -- Multi-driven assignments
  dltbefac <= 'X';
end us;



-- Seed after: 16874074818335541545,7311216359267151659
