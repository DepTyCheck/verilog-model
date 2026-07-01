-- Seed: 11575739191198839369,6882842853887419669

entity flokmhu is
  port (ne : out time);
end flokmhu;

architecture yp of flokmhu is
  
begin
  
end yp;

entity iukr is
  port (zshglbhu : in real);
end iukr;

architecture bikmvzda of iukr is
  signal uheuhrwnd : time;
  signal yryj : time;
begin
  lpqbbkxaz : entity work.flokmhu
    port map (ne => yryj);
  evqrk : entity work.flokmhu
    port map (ne => uheuhrwnd);
end bikmvzda;

entity ekssxid is
  port (ik : buffer integer; xqq : out bit_vector(1 downto 0));
end ekssxid;

architecture aysqtc of ekssxid is
  signal c : time;
  signal wbsnavt : time;
  signal pbnkvgfzjv : real;
begin
  osybuoexue : entity work.iukr
    port map (zshglbhu => pbnkvgfzjv);
  ez : entity work.flokmhu
    port map (ne => wbsnavt);
  ygydryujr : entity work.flokmhu
    port map (ne => c);
  
  -- Single-driven assignments
  xqq <= ('0', '0');
  ik <= 4_3_3_2_2;
end aysqtc;



-- Seed after: 11839086939881627075,6882842853887419669
