-- Seed: 2355642458570605111,4245627776430562977

library ieee;
use ieee.std_logic_1164.all;

entity rp is
  port (ydxi : buffer std_logic_vector(0 downto 4); cqnjz : buffer std_logic; efyq : inout std_logic_vector(2 downto 2); otnr : out std_logic);
end rp;

architecture sw of rp is
  
begin
  -- Multi-driven assignments
  otnr <= 'L';
  otnr <= otnr;
end sw;

entity oni is
  port (xafkoxcz : buffer time; zxmw : out bit_vector(4 downto 4));
end oni;

architecture fdjqa of oni is
  
begin
  -- Single-driven assignments
  xafkoxcz <= xafkoxcz;
  zxmw <= (others => '1');
end fdjqa;

library ieee;
use ieee.std_logic_1164.all;

entity idpklo is
  port (ctu : buffer std_logic);
end idpklo;

library ieee;
use ieee.std_logic_1164.all;

architecture wvcdiejxrr of idpklo is
  signal ecgqujniu : std_logic;
  signal lc : std_logic_vector(2 downto 2);
  signal mpjkhczpo : std_logic_vector(0 downto 4);
begin
  uvadfhyij : entity work.rp
    port map (ydxi => mpjkhczpo, cqnjz => ctu, efyq => lc, otnr => ecgqujniu);
  
  -- Multi-driven assignments
  ctu <= ctu;
  ecgqujniu <= ctu;
  ctu <= ecgqujniu;
  ctu <= ctu;
end wvcdiejxrr;

entity lvbgqugqzh is
  port (yqg : out integer; benjun : inout integer_vector(2 downto 2); urv : buffer character; n : inout integer);
end lvbgqugqzh;

library ieee;
use ieee.std_logic_1164.all;

architecture qpoit of lvbgqugqzh is
  signal wqvjmct : std_logic;
  signal xx : std_logic_vector(0 downto 4);
  signal lf : std_logic;
  signal vlkyxdbr : std_logic_vector(2 downto 2);
  signal uo : std_logic;
  signal hvgvzzbiw : std_logic_vector(2 downto 2);
  signal qtzk : std_logic;
  signal h : std_logic_vector(0 downto 4);
begin
  susdixqmhz : entity work.rp
    port map (ydxi => h, cqnjz => qtzk, efyq => hvgvzzbiw, otnr => qtzk);
  irntvz : entity work.rp
    port map (ydxi => h, cqnjz => uo, efyq => vlkyxdbr, otnr => lf);
  trj : entity work.rp
    port map (ydxi => xx, cqnjz => uo, efyq => hvgvzzbiw, otnr => wqvjmct);
  
  -- Single-driven assignments
  yqg <= n;
  n <= yqg;
  urv <= urv;
  
  -- Multi-driven assignments
  lf <= '-';
  h <= "";
  qtzk <= 'H';
  qtzk <= qtzk;
end qpoit;



-- Seed after: 16334566365795409463,4245627776430562977
