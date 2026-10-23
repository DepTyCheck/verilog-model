-- Seed: 16385859569663532163,6176025545472508329

library ieee;
use ieee.std_logic_1164.all;

entity wxez is
  port ( vluamnb : in integer_vector(3 downto 4)
  ; zurgpmc : in std_logic_vector(2 downto 2)
  ; w : out std_logic_vector(0 downto 2)
  ; uzrmyawguq : linkage std_logic
  );
end wxez;

architecture grfyogz of wxez is
  
begin
  
end grfyogz;

library ieee;
use ieee.std_logic_1164.all;

entity tvoh is
  port (sntvqdb : buffer integer; kl : linkage std_logic_vector(2 to 0));
end tvoh;

library ieee;
use ieee.std_logic_1164.all;

architecture ojp of tvoh is
  signal mryqayco : integer_vector(3 downto 4);
  signal agejlugbx : std_logic;
  signal ljdmqxizta : std_logic_vector(2 downto 2);
  signal lwtwsa : integer_vector(3 downto 4);
  signal s : std_logic;
  signal etdnbgrxm : std_logic_vector(0 downto 2);
  signal jp : std_logic_vector(2 downto 2);
  signal tqub : integer_vector(3 downto 4);
begin
  duvkauztat : entity work.wxez
    port map (vluamnb => tqub, zurgpmc => jp, w => etdnbgrxm, uzrmyawguq => s);
  bwo : entity work.wxez
    port map (vluamnb => lwtwsa, zurgpmc => ljdmqxizta, w => etdnbgrxm, uzrmyawguq => agejlugbx);
  iumkse : entity work.wxez
    port map (vluamnb => mryqayco, zurgpmc => jp, w => etdnbgrxm, uzrmyawguq => s);
  
  -- Single-driven assignments
  mryqayco <= tqub;
  lwtwsa <= (others => 0);
  sntvqdb <= sntvqdb;
  tqub <= mryqayco;
  
  -- Multi-driven assignments
  jp <= "W";
  ljdmqxizta <= (others => 'H');
end ojp;

entity c is
  port (r : buffer bit);
end c;

library ieee;
use ieee.std_logic_1164.all;

architecture rabtmhtopp of c is
  signal lbifo : std_logic;
  signal desthqys : std_logic_vector(0 downto 2);
  signal qdim : std_logic_vector(2 downto 2);
  signal lumkgmadkt : integer_vector(3 downto 4);
begin
  luwzl : entity work.wxez
    port map (vluamnb => lumkgmadkt, zurgpmc => qdim, w => desthqys, uzrmyawguq => lbifo);
  
  -- Multi-driven assignments
  lbifo <= lbifo;
end rabtmhtopp;



-- Seed after: 3388457902309753539,6176025545472508329
