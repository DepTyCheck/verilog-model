-- Seed: 12123626665087170466,6329330932550885447

library ieee;
use ieee.std_logic_1164.all;

entity jyznejx is
  port (qaiuo : linkage std_logic; anygcetmh : linkage std_logic; e : out integer_vector(2 to 4); cwfgxg : linkage time_vector(2 downto 3));
end jyznejx;



architecture dbc of jyznejx is
  
begin
  
end dbc;

library ieee;
use ieee.std_logic_1164.all;

entity e is
  port (dn : buffer std_logic_vector(2 to 2); mhzuhxgzd : linkage std_logic_vector(2 downto 3); puaqy : out integer);
end e;

library ieee;
use ieee.std_logic_1164.all;

architecture gzjcya of e is
  signal xy : time_vector(2 downto 3);
  signal qhczuavg : integer_vector(2 to 4);
  signal ix : std_logic;
  signal yjcjtdjrx : std_logic;
begin
  uyiyiqd : entity work.jyznejx
    port map (qaiuo => yjcjtdjrx, anygcetmh => ix, e => qhczuavg, cwfgxg => xy);
end gzjcya;

library ieee;
use ieee.std_logic_1164.all;

entity vgv is
  port (gpzujiox : linkage character; yvtlzcpdkr : linkage std_logic_vector(1 downto 3); ae : linkage integer_vector(2 to 4); hydq : buffer integer);
end vgv;

library ieee;
use ieee.std_logic_1164.all;

architecture qigtswfswd of vgv is
  signal afhv : integer;
  signal liokm : std_logic_vector(2 downto 3);
  signal urquhruh : std_logic_vector(2 to 2);
begin
  qcev : entity work.e
    port map (dn => urquhruh, mhzuhxgzd => liokm, puaqy => afhv);
end qigtswfswd;

library ieee;
use ieee.std_logic_1164.all;

entity mnwbc is
  port (avzgw : in bit; rhj : out std_logic);
end mnwbc;

library ieee;
use ieee.std_logic_1164.all;

architecture bmzkgwgin of mnwbc is
  signal rfe : integer_vector(2 to 4);
  signal ixgmtvpb : std_logic;
  signal wclam : time_vector(2 downto 3);
  signal yjpgb : std_logic;
  signal eiooxcau : time_vector(2 downto 3);
  signal xek : integer_vector(2 to 4);
  signal msjqvwg : integer;
  signal gjrsw : integer_vector(2 to 4);
  signal t : std_logic_vector(1 downto 3);
  signal oztojzumd : character;
begin
  ph : entity work.vgv
    port map (gpzujiox => oztojzumd, yvtlzcpdkr => t, ae => gjrsw, hydq => msjqvwg);
  wm : entity work.jyznejx
    port map (qaiuo => rhj, anygcetmh => rhj, e => xek, cwfgxg => eiooxcau);
  vtpp : entity work.jyznejx
    port map (qaiuo => rhj, anygcetmh => yjpgb, e => gjrsw, cwfgxg => wclam);
  wzpykl : entity work.jyznejx
    port map (qaiuo => ixgmtvpb, anygcetmh => rhj, e => rfe, cwfgxg => wclam);
end bmzkgwgin;



-- Seed after: 4372006397145435673,6329330932550885447
