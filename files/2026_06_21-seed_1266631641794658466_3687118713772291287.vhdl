-- Seed: 1266631641794658466,3687118713772291287

library ieee;
use ieee.std_logic_1164.all;

entity vop is
  port (zreaydl : in std_logic_vector(3 to 3); kisjnr : buffer boolean; nqieusgw : out boolean);
end vop;

architecture gmfwngwlbt of vop is
  
begin
  -- Single-driven assignments
  nqieusgw <= FALSE;
  kisjnr <= TRUE;
end gmfwngwlbt;

entity hvaduff is
  port (onuqtpjjfy : in severity_level; vzpoym : out integer_vector(4 downto 3); jtt : in integer; ikxpx : out boolean_vector(4 downto 1));
end hvaduff;

library ieee;
use ieee.std_logic_1164.all;

architecture mb of hvaduff is
  signal okgid : boolean;
  signal nyzbgt : boolean;
  signal bdivhtayz : boolean;
  signal zfxfhvqfel : boolean;
  signal xxdhgdq : std_logic_vector(3 to 3);
begin
  aziqllpzsu : entity work.vop
    port map (zreaydl => xxdhgdq, kisjnr => zfxfhvqfel, nqieusgw => bdivhtayz);
  tbkrb : entity work.vop
    port map (zreaydl => xxdhgdq, kisjnr => nyzbgt, nqieusgw => okgid);
  
  -- Single-driven assignments
  ikxpx <= (TRUE, FALSE, FALSE, TRUE);
end mb;

library ieee;
use ieee.std_logic_1164.all;

entity vhnr is
  port ( csmoito : buffer std_logic_vector(3 downto 2)
  ; bp : linkage std_logic_vector(4 to 1)
  ; xkzkya : buffer boolean
  ; hmetkbc : buffer std_logic_vector(0 to 1)
  );
end vhnr;

library ieee;
use ieee.std_logic_1164.all;

architecture dfblwh of vhnr is
  signal qhezjrjs : boolean;
  signal radopnzzkg : std_logic_vector(3 to 3);
begin
  qlfccg : entity work.vop
    port map (zreaydl => radopnzzkg, kisjnr => xkzkya, nqieusgw => qhezjrjs);
  
  -- Multi-driven assignments
  hmetkbc <= "1-";
end dfblwh;

library ieee;
use ieee.std_logic_1164.all;

entity aa is
  port (hl : linkage boolean_vector(4 downto 3); mnvdbm : linkage time; gosit : inout std_logic; uficl : buffer time);
end aa;

library ieee;
use ieee.std_logic_1164.all;

architecture ciu of aa is
  signal p : boolean;
  signal iqkdgwwn : boolean;
  signal zw : std_logic_vector(3 to 3);
  signal c : boolean_vector(4 downto 1);
  signal hzkxqxn : integer;
  signal wywaegscfq : integer_vector(4 downto 3);
  signal cowqwhrrxv : severity_level;
begin
  tesveajh : entity work.hvaduff
    port map (onuqtpjjfy => cowqwhrrxv, vzpoym => wywaegscfq, jtt => hzkxqxn, ikxpx => c);
  cokqafga : entity work.vop
    port map (zreaydl => zw, kisjnr => iqkdgwwn, nqieusgw => p);
  
  -- Multi-driven assignments
  gosit <= '1';
  gosit <= 'W';
  gosit <= 'X';
  zw <= (others => 'X');
end ciu;



-- Seed after: 4474389311111765588,3687118713772291287
