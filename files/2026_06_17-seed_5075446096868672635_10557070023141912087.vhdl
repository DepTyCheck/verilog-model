-- Seed: 5075446096868672635,10557070023141912087

library ieee;
use ieee.std_logic_1164.all;

entity sznowkv is
  port (qcizc : buffer std_logic; qynvtopkwb : linkage std_logic);
end sznowkv;

architecture bflluuys of sznowkv is
  
begin
  -- Multi-driven assignments
  qcizc <= '0';
  qcizc <= 'W';
  qcizc <= 'H';
  qcizc <= '0';
end bflluuys;

library ieee;
use ieee.std_logic_1164.all;

entity dovu is
  port (rx : out std_logic; vclswvnxl : in time; oosgeuez : inout time);
end dovu;

library ieee;
use ieee.std_logic_1164.all;

architecture gwn of dovu is
  signal aa : std_logic;
  signal jjsdtd : std_logic;
  signal pouaaritk : std_logic;
  signal yilca : std_logic;
begin
  y : entity work.sznowkv
    port map (qcizc => rx, qynvtopkwb => yilca);
  wodfymiu : entity work.sznowkv
    port map (qcizc => yilca, qynvtopkwb => pouaaritk);
  ocydazbfa : entity work.sznowkv
    port map (qcizc => jjsdtd, qynvtopkwb => aa);
  
  -- Single-driven assignments
  oosgeuez <= 8#1# fs;
  
  -- Multi-driven assignments
  rx <= 'L';
  pouaaritk <= 'X';
  yilca <= 'Z';
  aa <= 'Z';
end gwn;

library ieee;
use ieee.std_logic_1164.all;

entity oqge is
  port ( wuaivhwob : inout std_logic_vector(1 downto 1)
  ; xgfclh : linkage boolean
  ; okgwn : linkage std_logic_vector(4 to 0)
  ; eysmekov : buffer time_vector(3 to 3)
  );
end oqge;

library ieee;
use ieee.std_logic_1164.all;

architecture dlfjconoq of oqge is
  signal cmtz : time;
  signal caoh : std_logic;
  signal eqtnpnz : std_logic;
  signal q : std_logic;
  signal akhvflli : time;
  signal kjblr : time;
  signal zoqbfd : std_logic;
begin
  wprympkmc : entity work.dovu
    port map (rx => zoqbfd, vclswvnxl => kjblr, oosgeuez => akhvflli);
  kl : entity work.sznowkv
    port map (qcizc => q, qynvtopkwb => zoqbfd);
  dggi : entity work.sznowkv
    port map (qcizc => eqtnpnz, qynvtopkwb => q);
  zir : entity work.dovu
    port map (rx => caoh, vclswvnxl => kjblr, oosgeuez => cmtz);
  
  -- Single-driven assignments
  kjblr <= 8#1# ms;
  eysmekov <= (others => 1 min);
  
  -- Multi-driven assignments
  wuaivhwob <= "L";
  zoqbfd <= '-';
  zoqbfd <= 'W';
  wuaivhwob <= (others => 'L');
end dlfjconoq;



-- Seed after: 15877021343053756617,10557070023141912087
