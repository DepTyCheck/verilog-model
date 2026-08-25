-- Seed: 1843210869242619658,13501862637168280927

entity uffcsvqp is
  port (yxiophv : buffer integer; aiwgeraa : in time; fwthrhzgp : in character);
end uffcsvqp;

architecture rwzhuydaa of uffcsvqp is
  
begin
  -- Single-driven assignments
  yxiophv <= 8#7332#;
end rwzhuydaa;

library ieee;
use ieee.std_logic_1164.all;

entity sl is
  port (bmgbnih : buffer std_logic_vector(1 downto 4); so : buffer std_logic);
end sl;

architecture movjcxznws of sl is
  signal sn : character;
  signal kdkvndw : integer;
  signal en : integer;
  signal scpcp : time;
  signal sucezkk : integer;
  signal dgdsdklayi : character;
  signal dbhgbjdg : time;
  signal fwbvzvme : integer;
begin
  unnljvcubg : entity work.uffcsvqp
    port map (yxiophv => fwbvzvme, aiwgeraa => dbhgbjdg, fwthrhzgp => dgdsdklayi);
  pqg : entity work.uffcsvqp
    port map (yxiophv => sucezkk, aiwgeraa => scpcp, fwthrhzgp => dgdsdklayi);
  kcdbqyahy : entity work.uffcsvqp
    port map (yxiophv => en, aiwgeraa => dbhgbjdg, fwthrhzgp => dgdsdklayi);
  vxqkeitat : entity work.uffcsvqp
    port map (yxiophv => kdkvndw, aiwgeraa => scpcp, fwthrhzgp => sn);
  
  -- Single-driven assignments
  dgdsdklayi <= dgdsdklayi;
  sn <= dgdsdklayi;
  scpcp <= scpcp;
  dbhgbjdg <= dbhgbjdg;
  
  -- Multi-driven assignments
  so <= so;
  so <= '1';
end movjcxznws;

entity kgpfbys is
  port (wnl : inout boolean);
end kgpfbys;

library ieee;
use ieee.std_logic_1164.all;

architecture hdeps of kgpfbys is
  signal esd : time;
  signal kowspy : integer;
  signal roj : std_logic;
  signal psuweyuipf : std_logic_vector(1 downto 4);
  signal cxmlczv : character;
  signal fid : time;
  signal wsqo : integer;
begin
  xxgcjdo : entity work.uffcsvqp
    port map (yxiophv => wsqo, aiwgeraa => fid, fwthrhzgp => cxmlczv);
  rauluh : entity work.sl
    port map (bmgbnih => psuweyuipf, so => roj);
  qic : entity work.uffcsvqp
    port map (yxiophv => kowspy, aiwgeraa => esd, fwthrhzgp => cxmlczv);
  
  -- Single-driven assignments
  cxmlczv <= 'k';
  fid <= 8#2_3_4_2# ms;
  esd <= fid;
  wnl <= wnl;
  
  -- Multi-driven assignments
  psuweyuipf <= "";
end hdeps;



-- Seed after: 15323175659816321117,13501862637168280927
