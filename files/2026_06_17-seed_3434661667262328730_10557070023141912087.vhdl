-- Seed: 3434661667262328730,10557070023141912087

library ieee;
use ieee.std_logic_1164.all;

entity hp is
  port (wizfqdwhf : inout std_logic; qmgsyxibiw : out integer; o : inout integer; qky : out std_logic);
end hp;

architecture hnqrenesi of hp is
  
begin
  -- Single-driven assignments
  o <= 13143;
  qmgsyxibiw <= 2_1_0_1;
  
  -- Multi-driven assignments
  wizfqdwhf <= '0';
  qky <= 'H';
end hnqrenesi;

library ieee;
use ieee.std_logic_1164.all;

entity hxslqjfxe is
  port (jjlkwsuhwz : inout integer; divsyxnyv : inout std_logic; jzpqqwx : in std_logic; bybqea : inout integer);
end hxslqjfxe;

library ieee;
use ieee.std_logic_1164.all;

architecture w of hxslqjfxe is
  signal qtywwb : std_logic;
  signal yfntg : integer;
  signal kcx : integer;
  signal kvkhnkvmm : std_logic;
begin
  tdff : entity work.hp
    port map (wizfqdwhf => kvkhnkvmm, qmgsyxibiw => kcx, o => yfntg, qky => divsyxnyv);
  anb : entity work.hp
    port map (wizfqdwhf => kvkhnkvmm, qmgsyxibiw => bybqea, o => jjlkwsuhwz, qky => qtywwb);
  
  -- Multi-driven assignments
  divsyxnyv <= 'W';
  divsyxnyv <= 'Z';
  divsyxnyv <= '0';
end w;

library ieee;
use ieee.std_logic_1164.all;

entity vulcxwzusz is
  port (eqehhf : inout std_logic; ksibmigu : inout time; bimatlqbft : out real_vector(2 downto 3); viwmszs : buffer std_logic);
end vulcxwzusz;

library ieee;
use ieee.std_logic_1164.all;

architecture t of vulcxwzusz is
  signal wjwjirxcg : std_logic;
  signal scurnl : integer;
  signal haymxeuw : integer;
  signal hmwav : integer;
  signal brajancrur : std_logic;
  signal zeqpfkn : integer;
  signal xpzhojq : integer;
  signal cgb : integer;
  signal altmovu : std_logic;
begin
  pzya : entity work.hp
    port map (wizfqdwhf => altmovu, qmgsyxibiw => cgb, o => xpzhojq, qky => altmovu);
  phb : entity work.hxslqjfxe
    port map (jjlkwsuhwz => zeqpfkn, divsyxnyv => brajancrur, jzpqqwx => viwmszs, bybqea => hmwav);
  rfytdspu : entity work.hp
    port map (wizfqdwhf => viwmszs, qmgsyxibiw => haymxeuw, o => scurnl, qky => wjwjirxcg);
  
  -- Single-driven assignments
  ksibmigu <= 21 fs;
  
  -- Multi-driven assignments
  viwmszs <= 'L';
  eqehhf <= 'Z';
  viwmszs <= 'Z';
  viwmszs <= 'Z';
end t;



-- Seed after: 3168767883124446365,10557070023141912087
