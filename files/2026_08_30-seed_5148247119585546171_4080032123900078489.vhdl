-- Seed: 5148247119585546171,4080032123900078489

entity ddbou is
  port (gks : in time; vnsgai : out real; i : inout boolean_vector(1 downto 3));
end ddbou;

architecture bi of ddbou is
  
begin
  -- Single-driven assignments
  i <= i;
  vnsgai <= vnsgai;
end bi;

entity c is
  port (t : in boolean_vector(3 downto 2); ymukhiajp : inout boolean_vector(2 to 0));
end c;

architecture zhotren of c is
  signal mlvbtwz : boolean_vector(1 downto 3);
  signal lle : real;
  signal fcmgl : time;
begin
  pswqg : entity work.ddbou
    port map (gks => fcmgl, vnsgai => lle, i => mlvbtwz);
  
  -- Single-driven assignments
  ymukhiajp <= (others => TRUE);
end zhotren;

entity fgbiaexvq is
  port (ekioo : inout time; w : inout real);
end fgbiaexvq;

architecture avjtfagijp of fgbiaexvq is
  signal xv : boolean_vector(1 downto 3);
  signal lgu : boolean_vector(1 downto 3);
  signal fackgxm : real;
  signal x : boolean_vector(1 downto 3);
  signal qmtydsvy : real;
  signal iytlhqeb : time;
begin
  bhewssi : entity work.ddbou
    port map (gks => iytlhqeb, vnsgai => qmtydsvy, i => x);
  qano : entity work.ddbou
    port map (gks => ekioo, vnsgai => fackgxm, i => lgu);
  fmfya : entity work.ddbou
    port map (gks => ekioo, vnsgai => w, i => xv);
  
  -- Single-driven assignments
  ekioo <= 31244 us;
  iytlhqeb <= 2#1# us;
end avjtfagijp;

entity xh is
  port (mf : out integer);
end xh;

architecture hnbcfm of xh is
  signal ktvp : boolean_vector(1 downto 3);
  signal hwfhora : real;
  signal vph : time;
  signal usqiiolfdz : boolean_vector(2 to 0);
  signal sfspby : boolean_vector(3 downto 2);
begin
  yb : entity work.c
    port map (t => sfspby, ymukhiajp => usqiiolfdz);
  quu : entity work.ddbou
    port map (gks => vph, vnsgai => hwfhora, i => ktvp);
  
  -- Single-driven assignments
  mf <= mf;
  vph <= vph;
  sfspby <= sfspby;
end hnbcfm;



-- Seed after: 12406654597108712680,4080032123900078489
