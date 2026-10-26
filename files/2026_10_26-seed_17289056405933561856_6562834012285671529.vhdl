-- Seed: 17289056405933561856,6562834012285671529

entity uzqkaqu is
  port (mypifpivzh : out integer; edxnzdqt : out integer_vector(4 to 4));
end uzqkaqu;

architecture ovibv of uzqkaqu is
  
begin
  
end ovibv;

library ieee;
use ieee.std_logic_1164.all;

entity sg is
  port (yic : out std_logic; qihvrrddf : buffer real; obrqlsly : inout boolean_vector(4 to 1); vi : out boolean);
end sg;

architecture ii of sg is
  signal msfkrel : integer_vector(4 to 4);
  signal mtyefvmzj : integer;
  signal rdqhen : integer_vector(4 to 4);
  signal rlq : integer;
begin
  znngs : entity work.uzqkaqu
    port map (mypifpivzh => rlq, edxnzdqt => rdqhen);
  vhb : entity work.uzqkaqu
    port map (mypifpivzh => mtyefvmzj, edxnzdqt => msfkrel);
  
  -- Single-driven assignments
  vi <= FALSE;
  
  -- Multi-driven assignments
  yic <= yic;
  yic <= 'W';
  yic <= yic;
end ii;



-- Seed after: 5582169060028275385,6562834012285671529
