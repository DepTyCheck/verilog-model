-- Seed: 7207150036996120043,2287986438313874379

library ieee;
use ieee.std_logic_1164.all;

entity dupxcxbvsn is
  port (jp : linkage integer; pqzi : out std_logic; nt : in time_vector(3 downto 4));
end dupxcxbvsn;

architecture xc of dupxcxbvsn is
  
begin
  -- Multi-driven assignments
  pqzi <= '0';
end xc;

entity yddscba is
  port (fkgelepyir : inout integer; bcj : buffer time; xagq : in integer);
end yddscba;

library ieee;
use ieee.std_logic_1164.all;

architecture kovillgwac of yddscba is
  signal adasmrcu : time_vector(3 downto 4);
  signal eilbeseutt : std_logic;
begin
  lzeyumet : entity work.dupxcxbvsn
    port map (jp => fkgelepyir, pqzi => eilbeseutt, nt => adasmrcu);
  
  -- Single-driven assignments
  bcj <= bcj;
  adasmrcu <= adasmrcu;
  
  -- Multi-driven assignments
  eilbeseutt <= 'Z';
  eilbeseutt <= '0';
  eilbeseutt <= eilbeseutt;
end kovillgwac;

entity semlkchbo is
  port (fxpszv : in integer; epquhoisy : out boolean);
end semlkchbo;

library ieee;
use ieee.std_logic_1164.all;

architecture wanan of semlkchbo is
  signal v : integer;
  signal mbtknjlg : time_vector(3 downto 4);
  signal hnnj : std_logic;
  signal hpplwq : integer;
begin
  owkwmpjeau : entity work.dupxcxbvsn
    port map (jp => hpplwq, pqzi => hnnj, nt => mbtknjlg);
  kvyvjp : entity work.dupxcxbvsn
    port map (jp => v, pqzi => hnnj, nt => mbtknjlg);
  
  -- Single-driven assignments
  epquhoisy <= epquhoisy;
end wanan;

entity odfp is
  port (jjumnvjbty : linkage time; shjkwp : linkage time; eqraouvjq : out boolean_vector(3 downto 2));
end odfp;

library ieee;
use ieee.std_logic_1164.all;

architecture m of odfp is
  signal osjjwnsr : time_vector(3 downto 4);
  signal uratir : time_vector(3 downto 4);
  signal geobueaq : std_logic;
  signal xmidz : integer;
  signal jhpbn : time;
  signal ru : integer;
  signal qtvvzs : integer;
  signal qzmxkewuvs : time;
  signal bxgub : integer;
begin
  iakbxtmr : entity work.yddscba
    port map (fkgelepyir => bxgub, bcj => qzmxkewuvs, xagq => qtvvzs);
  pcdkfe : entity work.yddscba
    port map (fkgelepyir => ru, bcj => jhpbn, xagq => bxgub);
  ny : entity work.dupxcxbvsn
    port map (jp => xmidz, pqzi => geobueaq, nt => uratir);
  zvhk : entity work.dupxcxbvsn
    port map (jp => qtvvzs, pqzi => geobueaq, nt => osjjwnsr);
  
  -- Single-driven assignments
  eqraouvjq <= (FALSE, TRUE);
  
  -- Multi-driven assignments
  geobueaq <= geobueaq;
  geobueaq <= 'U';
  geobueaq <= geobueaq;
end m;



-- Seed after: 10078488143945620484,2287986438313874379
