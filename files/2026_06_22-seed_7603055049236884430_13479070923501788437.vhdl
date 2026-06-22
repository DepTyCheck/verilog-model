-- Seed: 7603055049236884430,13479070923501788437

entity djjfdjn is
  port (cuiftwznlj : out real; jikhtt : out real);
end djjfdjn;

architecture ymveyqi of djjfdjn is
  
begin
  -- Single-driven assignments
  cuiftwznlj <= 2#0.11#;
  jikhtt <= 23.2;
end ymveyqi;

library ieee;
use ieee.std_logic_1164.all;

entity xww is
  port (c : out std_logic; u : buffer integer; lytk : out std_logic_vector(4 downto 1));
end xww;

architecture igvfxakran of xww is
  signal acercziad : real;
  signal r : real;
  signal zely : real;
  signal zid : real;
  signal fsujwzegi : real;
  signal omiifkfbo : real;
begin
  zf : entity work.djjfdjn
    port map (cuiftwznlj => omiifkfbo, jikhtt => fsujwzegi);
  jqfn : entity work.djjfdjn
    port map (cuiftwznlj => zid, jikhtt => zely);
  zvponcr : entity work.djjfdjn
    port map (cuiftwznlj => r, jikhtt => acercziad);
end igvfxakran;

library ieee;
use ieee.std_logic_1164.all;

entity xzyqkmu is
  port (k : out character; fxvzkibaok : buffer std_logic; aihsq : in std_logic; eckowky : inout std_logic);
end xzyqkmu;

library ieee;
use ieee.std_logic_1164.all;

architecture abv of xzyqkmu is
  signal pptyiwjzvi : std_logic_vector(4 downto 1);
  signal tgr : integer;
  signal uectgmw : std_logic_vector(4 downto 1);
  signal xlcihdokv : integer;
begin
  cfunvwil : entity work.xww
    port map (c => eckowky, u => xlcihdokv, lytk => uectgmw);
  siigrue : entity work.xww
    port map (c => eckowky, u => tgr, lytk => pptyiwjzvi);
  
  -- Single-driven assignments
  k <= 't';
  
  -- Multi-driven assignments
  eckowky <= '1';
  uectgmw <= "HW-L";
  pptyiwjzvi <= ('Z', '1', '0', 'W');
end abv;



-- Seed after: 7765091957597071755,13479070923501788437
