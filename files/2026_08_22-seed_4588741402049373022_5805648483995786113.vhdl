-- Seed: 4588741402049373022,5805648483995786113

library ieee;
use ieee.std_logic_1164.all;

entity hquyhkmi is
  port (fzqxtlfz : linkage character; ejgocjqkyt : linkage std_logic; hbl : inout std_logic);
end hquyhkmi;

architecture wfolugydug of hquyhkmi is
  
begin
  
end wfolugydug;

entity trmixkkkin is
  port (cflsucop : in real);
end trmixkkkin;

architecture rolkwba of trmixkkkin is
  
begin
  
end rolkwba;

entity wfynaushm is
  port (etgcwpiocr : inout integer; q : out integer);
end wfynaushm;

architecture hucbwyek of wfynaushm is
  
begin
  -- Single-driven assignments
  q <= 16#D#;
  etgcwpiocr <= q;
end hucbwyek;

library ieee;
use ieee.std_logic_1164.all;

entity zajevkegmy is
  port (gxpkqjdi : in real; wbxdreq : inout std_logic; vvwhmaipdu : in std_logic_vector(0 to 4));
end zajevkegmy;

library ieee;
use ieee.std_logic_1164.all;

architecture mrue of zajevkegmy is
  signal zqk : character;
  signal ry : integer;
  signal q : integer;
  signal vw : std_logic;
  signal oetbbeco : std_logic;
  signal eiuc : character;
begin
  zs : entity work.hquyhkmi
    port map (fzqxtlfz => eiuc, ejgocjqkyt => oetbbeco, hbl => vw);
  izraheokw : entity work.wfynaushm
    port map (etgcwpiocr => q, q => ry);
  i : entity work.hquyhkmi
    port map (fzqxtlfz => zqk, ejgocjqkyt => wbxdreq, hbl => wbxdreq);
  
  -- Multi-driven assignments
  wbxdreq <= 'U';
  wbxdreq <= oetbbeco;
  oetbbeco <= wbxdreq;
  oetbbeco <= 'X';
end mrue;



-- Seed after: 5884786064803525874,5805648483995786113
