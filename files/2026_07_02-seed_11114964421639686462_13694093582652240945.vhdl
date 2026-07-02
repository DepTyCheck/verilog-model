-- Seed: 11114964421639686462,13694093582652240945

entity tidp is
  port (q : in integer_vector(0 to 2); rpbnbmu : buffer time; nlep : linkage real; ygdae : linkage time);
end tidp;

architecture qitstprua of tidp is
  
begin
  -- Single-driven assignments
  rpbnbmu <= 8#1# ms;
end qitstprua;

entity kspv is
  port (mdejut : buffer time; ybyffp : out boolean; hjamdbpaks : linkage time);
end kspv;

architecture lqyeoopaz of kspv is
  signal mvhvojxm : time;
  signal r : real;
  signal icchip : integer_vector(0 to 2);
  signal moadt : real;
  signal btrgqco : time;
  signal duaaeyvhj : time;
  signal n : real;
  signal k : time;
  signal w : time;
  signal fckv : real;
  signal czhzpmi : time;
  signal hvxubqvmo : integer_vector(0 to 2);
begin
  q : entity work.tidp
    port map (q => hvxubqvmo, rpbnbmu => czhzpmi, nlep => fckv, ygdae => w);
  fyk : entity work.tidp
    port map (q => hvxubqvmo, rpbnbmu => k, nlep => n, ygdae => duaaeyvhj);
  uesuqzzxf : entity work.tidp
    port map (q => hvxubqvmo, rpbnbmu => btrgqco, nlep => moadt, ygdae => hjamdbpaks);
  teu : entity work.tidp
    port map (q => icchip, rpbnbmu => mdejut, nlep => r, ygdae => mvhvojxm);
end lqyeoopaz;



-- Seed after: 12104623410984455410,13694093582652240945
