-- Seed: 4853916794828127076,3108530264173481209

entity lnktord is
  port (nqhjncbmu : buffer severity_level; aqfz : buffer real; ysayro : inout time_vector(0 downto 2));
end lnktord;

architecture wo of lnktord is
  
begin
  -- Single-driven assignments
  aqfz <= 4_2.2;
end wo;

entity idbhmn is
  port (yzrgeyf : linkage time; pcgprhawgf : buffer real);
end idbhmn;

architecture mbblbq of idbhmn is
  signal oopv : time_vector(0 downto 2);
  signal zesjaxw : severity_level;
  signal y : time_vector(0 downto 2);
  signal ecgpz : real;
  signal bwqbdrze : severity_level;
  signal wkuidwj : time_vector(0 downto 2);
  signal pbztrhcyyn : real;
  signal gmnetqy : severity_level;
  signal xlhhurki : time_vector(0 downto 2);
  signal pu : real;
  signal uty : severity_level;
begin
  otztgxkfq : entity work.lnktord
    port map (nqhjncbmu => uty, aqfz => pu, ysayro => xlhhurki);
  jtxhatl : entity work.lnktord
    port map (nqhjncbmu => gmnetqy, aqfz => pbztrhcyyn, ysayro => wkuidwj);
  yz : entity work.lnktord
    port map (nqhjncbmu => bwqbdrze, aqfz => ecgpz, ysayro => y);
  hcfcdvi : entity work.lnktord
    port map (nqhjncbmu => zesjaxw, aqfz => pcgprhawgf, ysayro => oopv);
end mbblbq;

entity bgcufr is
  port (yykrzu : out boolean_vector(2 downto 1));
end bgcufr;

architecture dyhj of bgcufr is
  signal fuawudj : time_vector(0 downto 2);
  signal ohxm : real;
  signal tqcfv : severity_level;
begin
  zgewhm : entity work.lnktord
    port map (nqhjncbmu => tqcfv, aqfz => ohxm, ysayro => fuawudj);
end dyhj;



-- Seed after: 8696718042471019848,3108530264173481209
