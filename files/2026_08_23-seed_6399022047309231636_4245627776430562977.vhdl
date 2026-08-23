-- Seed: 6399022047309231636,4245627776430562977

entity ogf is
  port (eymkszjhms : inout integer; czfzgtjh : in real);
end ogf;

architecture g of ogf is
  
begin
  -- Single-driven assignments
  eymkszjhms <= eymkszjhms;
end g;

entity umpqnnxk is
  port (qk : buffer bit);
end umpqnnxk;

architecture dryanzbv of umpqnnxk is
  signal zdexpfpyxe : real;
  signal utjwzkhaz : integer;
  signal jdjorbb : integer;
  signal svbpajc : real;
  signal wptxwjczi : integer;
begin
  ernnfwolip : entity work.ogf
    port map (eymkszjhms => wptxwjczi, czfzgtjh => svbpajc);
  jevnmb : entity work.ogf
    port map (eymkszjhms => jdjorbb, czfzgtjh => svbpajc);
  g : entity work.ogf
    port map (eymkszjhms => utjwzkhaz, czfzgtjh => zdexpfpyxe);
  
  -- Single-driven assignments
  qk <= '0';
  svbpajc <= 8#6_2_6_6.5_7_4_7_3#;
  zdexpfpyxe <= 2#1_0_1.10#;
end dryanzbv;

library ieee;
use ieee.std_logic_1164.all;

entity lnrdoivgu is
  port (cherx : buffer std_logic_vector(0 downto 0); ezqiway : out time; zxxypxvha : in std_logic_vector(4 to 0); zjgx : inout bit);
end lnrdoivgu;

architecture mgvbq of lnrdoivgu is
  
begin
  erdnzi : entity work.umpqnnxk
    port map (qk => zjgx);
end mgvbq;

library ieee;
use ieee.std_logic_1164.all;

entity cfmn is
  port (tgnbbqshat : buffer real; hcjbcwxgg : in character; cfgotwyf : buffer std_logic);
end cfmn;

library ieee;
use ieee.std_logic_1164.all;

architecture lmmels of cfmn is
  signal zocfwc : bit;
  signal iau : std_logic_vector(4 to 0);
  signal ecq : time;
  signal dbejhf : std_logic_vector(0 downto 0);
begin
  umvupkuzr : entity work.lnrdoivgu
    port map (cherx => dbejhf, ezqiway => ecq, zxxypxvha => iau, zjgx => zocfwc);
  
  -- Single-driven assignments
  tgnbbqshat <= tgnbbqshat;
  
  -- Multi-driven assignments
  cfgotwyf <= cfgotwyf;
end lmmels;



-- Seed after: 3856083153383977807,4245627776430562977
