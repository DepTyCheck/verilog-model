-- Seed: 18345157815222581443,3316342841050048249

library ieee;
use ieee.std_logic_1164.all;

entity wryn is
  port (wb : out boolean_vector(4 downto 0); lekaeub : out std_logic_vector(2 downto 1); cu : linkage integer; lmmcxac : out integer);
end wryn;

architecture up of wryn is
  
begin
  -- Multi-driven assignments
  lekaeub <= lekaeub;
  lekaeub <= lekaeub;
end up;

entity xgqwp is
  port (k : in real);
end xgqwp;

library ieee;
use ieee.std_logic_1164.all;

architecture sfvx of xgqwp is
  signal qyx : integer;
  signal t : integer;
  signal pbhiwu : boolean_vector(4 downto 0);
  signal rfrwas : integer;
  signal keobbehm : integer;
  signal e : boolean_vector(4 downto 0);
  signal icoyteii : integer;
  signal fxujcudgw : integer;
  signal tr : boolean_vector(4 downto 0);
  signal jkay : integer;
  signal jh : integer;
  signal zxl : std_logic_vector(2 downto 1);
  signal necypeej : boolean_vector(4 downto 0);
begin
  yxq : entity work.wryn
    port map (wb => necypeej, lekaeub => zxl, cu => jh, lmmcxac => jkay);
  uscf : entity work.wryn
    port map (wb => tr, lekaeub => zxl, cu => fxujcudgw, lmmcxac => icoyteii);
  aldv : entity work.wryn
    port map (wb => e, lekaeub => zxl, cu => keobbehm, lmmcxac => rfrwas);
  fc : entity work.wryn
    port map (wb => pbhiwu, lekaeub => zxl, cu => t, lmmcxac => qyx);
end sfvx;

library ieee;
use ieee.std_logic_1164.all;

entity chbkmpgz is
  port (yfkosrfvc : buffer boolean_vector(3 to 3); gapiazdlon : in integer; lmf : buffer real; urovwrkupn : out std_logic_vector(4 to 2));
end chbkmpgz;

architecture dccmhuchra of chbkmpgz is
  signal gu : real;
begin
  pum : entity work.xgqwp
    port map (k => gu);
  
  -- Single-driven assignments
  lmf <= lmf;
  gu <= lmf;
  yfkosrfvc <= (others => TRUE);
end dccmhuchra;

entity kze is
  port (jjmwsuh : buffer time);
end kze;

library ieee;
use ieee.std_logic_1164.all;

architecture ik of kze is
  signal bpnfpg : std_logic_vector(4 to 2);
  signal kqrlhiiu : real;
  signal wknl : boolean_vector(3 to 3);
  signal ehpbkduono : integer;
  signal flskx : integer;
  signal zbak : std_logic_vector(2 downto 1);
  signal zsnxvft : boolean_vector(4 downto 0);
begin
  czyiv : entity work.wryn
    port map (wb => zsnxvft, lekaeub => zbak, cu => flskx, lmmcxac => ehpbkduono);
  sjtc : entity work.chbkmpgz
    port map (yfkosrfvc => wknl, gapiazdlon => flskx, lmf => kqrlhiiu, urovwrkupn => bpnfpg);
  
  -- Single-driven assignments
  jjmwsuh <= jjmwsuh;
  
  -- Multi-driven assignments
  zbak <= zbak;
  bpnfpg <= (others => '0');
  zbak <= "-H";
end ik;



-- Seed after: 3711615426435166312,3316342841050048249
