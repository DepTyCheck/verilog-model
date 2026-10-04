-- Seed: 12516898227472893446,15795020531041709203

entity mfe is
  port (xvrys : buffer real; sh : linkage integer; mzqgyki : linkage real; dua : inout boolean_vector(1 downto 4));
end mfe;

architecture tvwxxfrd of mfe is
  
begin
  -- Single-driven assignments
  xvrys <= 4.0_3;
  dua <= (others => TRUE);
end tvwxxfrd;

library ieee;
use ieee.std_logic_1164.all;

entity mkk is
  port (mykm : out std_logic_vector(0 to 0); q : buffer integer);
end mkk;

architecture syozcpartv of mkk is
  signal rvolridzwy : boolean_vector(1 downto 4);
  signal veiefzzj : real;
  signal pmjokwmrhr : real;
  signal escuzollb : boolean_vector(1 downto 4);
  signal lwkr : real;
  signal jelhuoduja : integer;
  signal m : real;
  signal eaz : boolean_vector(1 downto 4);
  signal gdb : real;
  signal tt : integer;
  signal hbzuqat : real;
  signal ov : boolean_vector(1 downto 4);
  signal u : real;
  signal thjlqx : integer;
  signal gvfddbxge : real;
begin
  s : entity work.mfe
    port map (xvrys => gvfddbxge, sh => thjlqx, mzqgyki => u, dua => ov);
  bb : entity work.mfe
    port map (xvrys => hbzuqat, sh => tt, mzqgyki => gdb, dua => eaz);
  zzccradrh : entity work.mfe
    port map (xvrys => m, sh => jelhuoduja, mzqgyki => lwkr, dua => escuzollb);
  palyjnfhl : entity work.mfe
    port map (xvrys => pmjokwmrhr, sh => q, mzqgyki => veiefzzj, dua => rvolridzwy);
  
  -- Multi-driven assignments
  mykm <= (others => 'L');
  mykm <= mykm;
  mykm <= mykm;
  mykm <= mykm;
end syozcpartv;



-- Seed after: 12030796329277753690,15795020531041709203
