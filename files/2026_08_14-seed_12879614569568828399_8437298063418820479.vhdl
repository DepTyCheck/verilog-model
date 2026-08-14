-- Seed: 12879614569568828399,8437298063418820479

library ieee;
use ieee.std_logic_1164.all;

entity qn is
  port (o : buffer bit_vector(3 to 4); fu : in time; lbjiwxr : out std_logic);
end qn;

architecture cuydxrmbv of qn is
  
begin
  -- Single-driven assignments
  o <= ('1', '0');
  
  -- Multi-driven assignments
  lbjiwxr <= lbjiwxr;
  lbjiwxr <= lbjiwxr;
  lbjiwxr <= '0';
end cuydxrmbv;

library ieee;
use ieee.std_logic_1164.all;

entity cbvgm is
  port (luter : in boolean; vxxya : out std_logic_vector(2 to 4); yfiuf : in time);
end cbvgm;

library ieee;
use ieee.std_logic_1164.all;

architecture ubsbv of cbvgm is
  signal nvrapu : time;
  signal ntetxbgacz : bit_vector(3 to 4);
  signal kcnqqzr : std_logic;
  signal wg : bit_vector(3 to 4);
  signal udiea : std_logic;
  signal jtmdxc : time;
  signal xsnzso : bit_vector(3 to 4);
begin
  bwqe : entity work.qn
    port map (o => xsnzso, fu => jtmdxc, lbjiwxr => udiea);
  bvztscefw : entity work.qn
    port map (o => wg, fu => jtmdxc, lbjiwxr => kcnqqzr);
  eeksqgbl : entity work.qn
    port map (o => ntetxbgacz, fu => nvrapu, lbjiwxr => udiea);
  
  -- Multi-driven assignments
  vxxya <= ('U', '-', '1');
  udiea <= '0';
end ubsbv;

library ieee;
use ieee.std_logic_1164.all;

entity poslmnmlt is
  port (oaib : inout std_logic_vector(4 downto 4); zlvuli : buffer time; fijybkap : out bit; v : inout severity_level);
end poslmnmlt;

library ieee;
use ieee.std_logic_1164.all;

architecture ljw of poslmnmlt is
  signal opdx : std_logic;
  signal paigt : time;
  signal lgcapbtyud : bit_vector(3 to 4);
  signal qoznvnmk : std_logic;
  signal ckjtqto : time;
  signal ol : bit_vector(3 to 4);
  signal uy : std_logic;
  signal ommbos : time;
  signal rzv : bit_vector(3 to 4);
begin
  gozsa : entity work.qn
    port map (o => rzv, fu => ommbos, lbjiwxr => uy);
  by : entity work.qn
    port map (o => ol, fu => ckjtqto, lbjiwxr => qoznvnmk);
  zrpywbsvq : entity work.qn
    port map (o => lgcapbtyud, fu => paigt, lbjiwxr => opdx);
  
  -- Single-driven assignments
  v <= v;
  fijybkap <= fijybkap;
  zlvuli <= 1 hr;
  
  -- Multi-driven assignments
  oaib <= (others => 'U');
  uy <= uy;
  oaib <= oaib;
end ljw;



-- Seed after: 16070279229132303092,8437298063418820479
