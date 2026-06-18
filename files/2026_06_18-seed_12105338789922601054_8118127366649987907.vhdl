-- Seed: 12105338789922601054,8118127366649987907

library ieee;
use ieee.std_logic_1164.all;

entity zanzo is
  port (dukmbb : buffer character; r : out std_logic; fakksh : in real; atjzgr : inout integer);
end zanzo;

architecture oydjamag of zanzo is
  
begin
  -- Multi-driven assignments
  r <= 'W';
  r <= 'Z';
  r <= '-';
end oydjamag;

entity qxgztlee is
  port (kcbsbkfhcs : out real);
end qxgztlee;

library ieee;
use ieee.std_logic_1164.all;

architecture yit of qxgztlee is
  signal wpwlyqis : integer;
  signal gyhi : character;
  signal iu : integer;
  signal gkqmu : character;
  signal zhidh : integer;
  signal kvtxdeqghl : real;
  signal xkq : character;
  signal wqyml : integer;
  signal kavjlcjlw : std_logic;
  signal nmu : character;
begin
  zpdacern : entity work.zanzo
    port map (dukmbb => nmu, r => kavjlcjlw, fakksh => kcbsbkfhcs, atjzgr => wqyml);
  yuhrinn : entity work.zanzo
    port map (dukmbb => xkq, r => kavjlcjlw, fakksh => kvtxdeqghl, atjzgr => zhidh);
  w : entity work.zanzo
    port map (dukmbb => gkqmu, r => kavjlcjlw, fakksh => kcbsbkfhcs, atjzgr => iu);
  dfjngs : entity work.zanzo
    port map (dukmbb => gyhi, r => kavjlcjlw, fakksh => kvtxdeqghl, atjzgr => wpwlyqis);
  
  -- Single-driven assignments
  kcbsbkfhcs <= 2#1_0_1.101#;
  kvtxdeqghl <= 8#4_0_0_5_4.6573#;
end yit;

library ieee;
use ieee.std_logic_1164.all;

entity pg is
  port (wzmspywyk : in bit_vector(4 downto 3); jgdgclfhy : in std_logic; wdauv : in integer_vector(0 to 1));
end pg;

library ieee;
use ieee.std_logic_1164.all;

architecture lcpuyw of pg is
  signal uzmtkobpdt : real;
  signal dnexo : integer;
  signal wje : real;
  signal cuvvu : std_logic;
  signal blfctewoa : character;
  signal zwtfyc : real;
begin
  ruouxh : entity work.qxgztlee
    port map (kcbsbkfhcs => zwtfyc);
  kvq : entity work.zanzo
    port map (dukmbb => blfctewoa, r => cuvvu, fakksh => wje, atjzgr => dnexo);
  kbru : entity work.qxgztlee
    port map (kcbsbkfhcs => uzmtkobpdt);
  gebi : entity work.qxgztlee
    port map (kcbsbkfhcs => wje);
  
  -- Multi-driven assignments
  cuvvu <= 'W';
  cuvvu <= 'X';
  cuvvu <= 'U';
end lcpuyw;



-- Seed after: 2441239772663780815,8118127366649987907
