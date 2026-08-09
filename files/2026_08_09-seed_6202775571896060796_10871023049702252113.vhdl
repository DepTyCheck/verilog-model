-- Seed: 6202775571896060796,10871023049702252113

entity etpnnyypfo is
  port (uey : out boolean);
end etpnnyypfo;

architecture ovirut of etpnnyypfo is
  
begin
  -- Single-driven assignments
  uey <= FALSE;
end ovirut;

entity jvsxcglxs is
  port (x : out time);
end jvsxcglxs;

architecture dazz of jvsxcglxs is
  signal rzwv : boolean;
  signal fdqa : boolean;
  signal mevf : boolean;
begin
  hdtjkutz : entity work.etpnnyypfo
    port map (uey => mevf);
  xllpaeaehh : entity work.etpnnyypfo
    port map (uey => fdqa);
  hkjl : entity work.etpnnyypfo
    port map (uey => rzwv);
end dazz;

entity edodr is
  port (eqizcyavj : in real; qthnxartgg : inout integer; wrlljvkvd : linkage real_vector(2 to 1));
end edodr;

architecture aarsu of edodr is
  signal pt : boolean;
  signal t : time;
  signal o : boolean;
  signal blopgw : time;
begin
  xhuvjkiki : entity work.jvsxcglxs
    port map (x => blopgw);
  tkjws : entity work.etpnnyypfo
    port map (uey => o);
  swyxvwjkx : entity work.jvsxcglxs
    port map (x => t);
  irk : entity work.etpnnyypfo
    port map (uey => pt);
  
  -- Single-driven assignments
  qthnxartgg <= qthnxartgg;
end aarsu;

library ieee;
use ieee.std_logic_1164.all;

entity vaxrqhast is
  port (bexse : out std_logic; kdbmp : linkage std_logic_vector(2 to 2); swtubu : out real; shiue : in real);
end vaxrqhast;

architecture mgrltyu of vaxrqhast is
  signal ezyodtw : real_vector(2 to 1);
  signal b : integer;
  signal dzaemi : real;
  signal bmkpdisqb : real_vector(2 to 1);
  signal yqsa : integer;
  signal zck : real;
  signal s : real_vector(2 to 1);
  signal sn : integer;
  signal gashyto : real;
begin
  mwqasgk : entity work.edodr
    port map (eqizcyavj => gashyto, qthnxartgg => sn, wrlljvkvd => s);
  djpf : entity work.edodr
    port map (eqizcyavj => zck, qthnxartgg => yqsa, wrlljvkvd => bmkpdisqb);
  i : entity work.edodr
    port map (eqizcyavj => dzaemi, qthnxartgg => b, wrlljvkvd => ezyodtw);
  
  -- Single-driven assignments
  swtubu <= dzaemi;
  dzaemi <= 2#0.0_1_0#;
  zck <= 2_2_4.1_1_1_3_4;
  gashyto <= 23.3_0;
  
  -- Multi-driven assignments
  bexse <= '1';
  bexse <= bexse;
  bexse <= '-';
end mgrltyu;



-- Seed after: 17964009163166906972,10871023049702252113
