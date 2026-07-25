-- Seed: 13374643480576469633,5306691039457971049

library ieee;
use ieee.std_logic_1164.all;

entity dxfmrp is
  port (vpqp : in integer; xudklke : inout std_logic_vector(1 downto 2); dthf : inout real);
end dxfmrp;

architecture grzukpolur of dxfmrp is
  
begin
  -- Single-driven assignments
  dthf <= 16#7_4_2_F_8.3_8_0_C_F#;
  
  -- Multi-driven assignments
  xudklke <= (others => '0');
  xudklke <= xudklke;
  xudklke <= "";
end grzukpolur;

library ieee;
use ieee.std_logic_1164.all;

entity kbqvar is
  port ( fcmudgodo : in std_logic_vector(3 to 2)
  ; fy : in time_vector(1 downto 1)
  ; lcbab : in std_logic_vector(2 to 4)
  ; mpjqgp : inout boolean_vector(2 to 1)
  );
end kbqvar;

library ieee;
use ieee.std_logic_1164.all;

architecture yrna of kbqvar is
  signal dnjkpaoh : real;
  signal llss : integer;
  signal ow : real;
  signal fhb : std_logic_vector(1 downto 2);
  signal sfyt : integer;
begin
  wytum : entity work.dxfmrp
    port map (vpqp => sfyt, xudklke => fhb, dthf => ow);
  q : entity work.dxfmrp
    port map (vpqp => llss, xudklke => fhb, dthf => dnjkpaoh);
  
  -- Multi-driven assignments
  fhb <= fcmudgodo;
  fhb <= "";
  fhb <= fcmudgodo;
end yrna;

library ieee;
use ieee.std_logic_1164.all;

entity ciclwjnr is
  port (t : out std_logic_vector(0 to 4));
end ciclwjnr;

library ieee;
use ieee.std_logic_1164.all;

architecture vtslzjjm of ciclwjnr is
  signal l : boolean_vector(2 to 1);
  signal qbcpuq : std_logic_vector(2 to 4);
  signal cs : time_vector(1 downto 1);
  signal sayltki : real;
  signal zicbhz : std_logic_vector(3 to 2);
  signal ya : integer;
begin
  vxu : entity work.dxfmrp
    port map (vpqp => ya, xudklke => zicbhz, dthf => sayltki);
  yxtuu : entity work.kbqvar
    port map (fcmudgodo => zicbhz, fy => cs, lcbab => qbcpuq, mpjqgp => l);
  
  -- Single-driven assignments
  ya <= ya;
  cs <= cs;
  
  -- Multi-driven assignments
  t <= t;
  t <= ('W', 'L', 'H', 'W', '0');
end vtslzjjm;



-- Seed after: 4114677718243939635,5306691039457971049
