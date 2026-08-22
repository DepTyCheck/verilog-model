-- Seed: 1389457414882653137,5805648483995786113

entity lbif is
  port (m : inout boolean_vector(1 to 3); jzbyiscg : buffer bit; pcrj : linkage string(1 to 1); tfkdvc : out integer);
end lbif;

architecture dkfbhfa of lbif is
  
begin
  -- Single-driven assignments
  jzbyiscg <= '1';
  tfkdvc <= 1_1;
  m <= m;
end dkfbhfa;

library ieee;
use ieee.std_logic_1164.all;

entity vtqujuma is
  port (duey : linkage std_logic_vector(4 downto 3); midsd : linkage boolean; b : out severity_level; cswjhmy : out integer);
end vtqujuma;

architecture qt of vtqujuma is
  signal pqd : integer;
  signal ffiienj : string(1 to 1);
  signal rpmrhrqhp : bit;
  signal a : boolean_vector(1 to 3);
  signal kwsywrczm : string(1 to 1);
  signal njyun : bit;
  signal ulws : boolean_vector(1 to 3);
  signal upr : integer;
  signal cholucagvr : string(1 to 1);
  signal ygeuwpv : bit;
  signal fxslkfyel : boolean_vector(1 to 3);
begin
  vnhwulokki : entity work.lbif
    port map (m => fxslkfyel, jzbyiscg => ygeuwpv, pcrj => cholucagvr, tfkdvc => upr);
  ftdopvaab : entity work.lbif
    port map (m => ulws, jzbyiscg => njyun, pcrj => kwsywrczm, tfkdvc => cswjhmy);
  wqvgibou : entity work.lbif
    port map (m => a, jzbyiscg => rpmrhrqhp, pcrj => ffiienj, tfkdvc => pqd);
  
  -- Single-driven assignments
  b <= NOTE;
end qt;

entity txs is
  port (pynsq : buffer bit_vector(0 to 3); pph : out integer; ds : buffer time_vector(0 downto 2));
end txs;

library ieee;
use ieee.std_logic_1164.all;

architecture zd of txs is
  signal hsuluj : integer;
  signal o : string(1 to 1);
  signal hjh : bit;
  signal olagpdnne : boolean_vector(1 to 3);
  signal k : severity_level;
  signal mqxurx : boolean;
  signal ysipwjsahw : std_logic_vector(4 downto 3);
  signal nvn : integer;
  signal l : string(1 to 1);
  signal cnsrm : bit;
  signal nmww : boolean_vector(1 to 3);
  signal latpge : integer;
  signal jgdzrz : severity_level;
  signal uglr : boolean;
  signal qzxhuqdf : std_logic_vector(4 downto 3);
begin
  s : entity work.vtqujuma
    port map (duey => qzxhuqdf, midsd => uglr, b => jgdzrz, cswjhmy => latpge);
  yc : entity work.lbif
    port map (m => nmww, jzbyiscg => cnsrm, pcrj => l, tfkdvc => nvn);
  gij : entity work.vtqujuma
    port map (duey => ysipwjsahw, midsd => mqxurx, b => k, cswjhmy => pph);
  ngzfjj : entity work.lbif
    port map (m => olagpdnne, jzbyiscg => hjh, pcrj => o, tfkdvc => hsuluj);
  
  -- Single-driven assignments
  ds <= ds;
  pynsq <= ('1', '0', '0', '1');
  
  -- Multi-driven assignments
  qzxhuqdf <= "W-";
  qzxhuqdf <= qzxhuqdf;
  qzxhuqdf <= qzxhuqdf;
end zd;



-- Seed after: 728236274436067918,5805648483995786113
