-- Seed: 16330493817653169735,16188444798499499427

library ieee;
use ieee.std_logic_1164.all;

entity gsu is
  port (ksgu : linkage bit_vector(0 downto 3); kclwjg : out std_logic_vector(1 to 4); ezoqiiij : out character; twhbzkyr : out integer);
end gsu;

architecture qwdluyxwk of gsu is
  
begin
  -- Single-driven assignments
  ezoqiiij <= 'c';
  twhbzkyr <= 16#792FD#;
  
  -- Multi-driven assignments
  kclwjg <= kclwjg;
  kclwjg <= kclwjg;
  kclwjg <= ('H', 'L', '-', '0');
end qwdluyxwk;

entity ptfry is
  port (uk : in real);
end ptfry;

library ieee;
use ieee.std_logic_1164.all;

architecture qnhx of ptfry is
  signal tz : integer;
  signal buwx : character;
  signal qgf : bit_vector(0 downto 3);
  signal dxvxand : integer;
  signal jdmuegorh : character;
  signal bdtij : bit_vector(0 downto 3);
  signal btpetxc : integer;
  signal pjgeypmut : character;
  signal ellfa : std_logic_vector(1 to 4);
  signal neyzatdh : bit_vector(0 downto 3);
begin
  gtui : entity work.gsu
    port map (ksgu => neyzatdh, kclwjg => ellfa, ezoqiiij => pjgeypmut, twhbzkyr => btpetxc);
  qhmb : entity work.gsu
    port map (ksgu => bdtij, kclwjg => ellfa, ezoqiiij => jdmuegorh, twhbzkyr => dxvxand);
  gsb : entity work.gsu
    port map (ksgu => qgf, kclwjg => ellfa, ezoqiiij => buwx, twhbzkyr => tz);
  
  -- Multi-driven assignments
  ellfa <= "UW1-";
  ellfa <= ('0', '1', 'U', 'H');
  ellfa <= ellfa;
  ellfa <= ('1', 'H', 'Z', 'W');
end qnhx;



-- Seed after: 1737545014605893165,16188444798499499427
