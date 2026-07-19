-- Seed: 154649547190303383,5511103086789671269

library ieee;
use ieee.std_logic_1164.all;

entity flmuu is
  port (lxyqnm : out std_logic_vector(2 to 0); piyvy : inout integer; kilpykx : in boolean_vector(1 to 4));
end flmuu;

architecture ak of flmuu is
  
begin
  -- Single-driven assignments
  piyvy <= piyvy;
end ak;

entity gplygca is
  port (uxjzpm : linkage time_vector(0 downto 0); pcdxi : in bit; m : in real; vu : out real);
end gplygca;

architecture kcevptt of gplygca is
  
begin
  -- Single-driven assignments
  vu <= vu;
end kcevptt;

entity qjvjzo is
  port (ynw : in time);
end qjvjzo;

library ieee;
use ieee.std_logic_1164.all;

architecture kfys of qjvjzo is
  signal t : boolean_vector(1 to 4);
  signal gtdse : integer;
  signal zcbn : boolean_vector(1 to 4);
  signal exxr : integer;
  signal cepd : std_logic_vector(2 to 0);
  signal iast : integer;
  signal scesik : boolean_vector(1 to 4);
  signal xbrwrkzqhs : integer;
  signal orsx : std_logic_vector(2 to 0);
begin
  enimtido : entity work.flmuu
    port map (lxyqnm => orsx, piyvy => xbrwrkzqhs, kilpykx => scesik);
  tbyktl : entity work.flmuu
    port map (lxyqnm => orsx, piyvy => iast, kilpykx => scesik);
  qq : entity work.flmuu
    port map (lxyqnm => cepd, piyvy => exxr, kilpykx => zcbn);
  gmw : entity work.flmuu
    port map (lxyqnm => orsx, piyvy => gtdse, kilpykx => t);
  
  -- Single-driven assignments
  scesik <= (FALSE, FALSE, TRUE, FALSE);
  
  -- Multi-driven assignments
  orsx <= (others => '0');
  cepd <= "";
end kfys;

library ieee;
use ieee.std_logic_1164.all;

entity kwxnej is
  port (rg : out time; ulwlnoq : linkage std_logic);
end kwxnej;

library ieee;
use ieee.std_logic_1164.all;

architecture nrpfqxvo of kwxnej is
  signal gggvxmbud : time;
  signal kzzmn : real;
  signal wosawel : real;
  signal ybuhojoyph : bit;
  signal cah : time_vector(0 downto 0);
  signal aju : time;
  signal mpfav : boolean_vector(1 to 4);
  signal kmjgsduxms : integer;
  signal hytbofal : std_logic_vector(2 to 0);
begin
  mmjnrm : entity work.flmuu
    port map (lxyqnm => hytbofal, piyvy => kmjgsduxms, kilpykx => mpfav);
  xtiwszafnt : entity work.qjvjzo
    port map (ynw => aju);
  mfhy : entity work.gplygca
    port map (uxjzpm => cah, pcdxi => ybuhojoyph, m => wosawel, vu => kzzmn);
  rk : entity work.qjvjzo
    port map (ynw => gggvxmbud);
  
  -- Multi-driven assignments
  hytbofal <= hytbofal;
  hytbofal <= hytbofal;
end nrpfqxvo;



-- Seed after: 10879534811550079573,5511103086789671269
