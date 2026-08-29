-- Seed: 3964992566233275486,10463297573877745897

library ieee;
use ieee.std_logic_1164.all;

entity hedmomkyin is
  port (rfzmj : linkage character; s : out std_logic; utd : in boolean_vector(3 downto 4));
end hedmomkyin;

architecture lzio of hedmomkyin is
  
begin
  -- Multi-driven assignments
  s <= '0';
  s <= 'W';
end lzio;

library ieee;
use ieee.std_logic_1164.all;

entity dwpysj is
  port (ziwtuh : out std_logic);
end dwpysj;

library ieee;
use ieee.std_logic_1164.all;

architecture otfgsrv of dwpysj is
  signal ywwyfz : boolean_vector(3 downto 4);
  signal uaeqtvzje : std_logic;
  signal mtrbivn : character;
  signal mjzfg : boolean_vector(3 downto 4);
  signal eqepimbmnd : std_logic;
  signal yoom : character;
begin
  ndhwso : entity work.hedmomkyin
    port map (rfzmj => yoom, s => eqepimbmnd, utd => mjzfg);
  fldxvxddg : entity work.hedmomkyin
    port map (rfzmj => mtrbivn, s => uaeqtvzje, utd => ywwyfz);
  
  -- Single-driven assignments
  mjzfg <= (others => TRUE);
  ywwyfz <= mjzfg;
  
  -- Multi-driven assignments
  ziwtuh <= 'H';
  uaeqtvzje <= 'L';
  ziwtuh <= '-';
  eqepimbmnd <= 'U';
end otfgsrv;

entity dvw is
  port (gt : out character; umrbtsslq : out time; jr : in real);
end dvw;

library ieee;
use ieee.std_logic_1164.all;

architecture q of dvw is
  signal cgoyzwobt : boolean_vector(3 downto 4);
  signal uxhikrb : std_logic;
begin
  czggigtvse : entity work.hedmomkyin
    port map (rfzmj => gt, s => uxhikrb, utd => cgoyzwobt);
  
  -- Single-driven assignments
  umrbtsslq <= 1 ms;
  cgoyzwobt <= cgoyzwobt;
  
  -- Multi-driven assignments
  uxhikrb <= 'Z';
end q;



-- Seed after: 4216372887742628444,10463297573877745897
