-- Seed: 13402085624117868885,12269339630485015285

entity ctvtdjdac is
  port (siigj : in time; meksfih : out integer_vector(0 to 2));
end ctvtdjdac;

architecture fajpptq of ctvtdjdac is
  
begin
  -- Single-driven assignments
  meksfih <= meksfih;
end fajpptq;

entity nfbwntug is
  port (xgkk : out integer_vector(0 downto 1); vj : inout integer);
end nfbwntug;

architecture qxzdxlbms of nfbwntug is
  signal fxqa : integer_vector(0 to 2);
  signal tienm : integer_vector(0 to 2);
  signal lhlidjicvp : time;
  signal zj : integer_vector(0 to 2);
  signal gzefajh : time;
begin
  orgoff : entity work.ctvtdjdac
    port map (siigj => gzefajh, meksfih => zj);
  kwkpkao : entity work.ctvtdjdac
    port map (siigj => lhlidjicvp, meksfih => tienm);
  ejpnttgnd : entity work.ctvtdjdac
    port map (siigj => gzefajh, meksfih => fxqa);
end qxzdxlbms;

library ieee;
use ieee.std_logic_1164.all;

entity kz is
  port (fmavgdst : linkage std_logic; fcajmrdsu : out time_vector(3 to 2); fq : linkage boolean; qkq : inout std_logic_vector(3 to 0));
end kz;

architecture dcxc of kz is
  signal zpleab : integer_vector(0 to 2);
  signal nwepe : time;
  signal yvrgtbb : integer_vector(0 to 2);
  signal kqmz : integer_vector(0 to 2);
  signal ddwdormw : time;
  signal byk : integer;
  signal lr : integer_vector(0 downto 1);
begin
  p : entity work.nfbwntug
    port map (xgkk => lr, vj => byk);
  yf : entity work.ctvtdjdac
    port map (siigj => ddwdormw, meksfih => kqmz);
  rgjm : entity work.ctvtdjdac
    port map (siigj => ddwdormw, meksfih => yvrgtbb);
  nlgcpdr : entity work.ctvtdjdac
    port map (siigj => nwepe, meksfih => zpleab);
  
  -- Single-driven assignments
  ddwdormw <= ddwdormw;
  fcajmrdsu <= (others => 0 ns);
  nwepe <= 3_4_0_2 us;
  
  -- Multi-driven assignments
  qkq <= "";
  qkq <= "";
  qkq <= qkq;
  qkq <= qkq;
end dcxc;

library ieee;
use ieee.std_logic_1164.all;

entity swlhteal is
  port (mvbhzouaf : buffer time_vector(1 downto 0); q : inout std_logic; vprcbduhjj : in time);
end swlhteal;

architecture dtzn of swlhteal is
  signal tghekizol : integer_vector(0 to 2);
  signal kmxrqsyw : time;
  signal cr : integer_vector(0 to 2);
  signal r : time;
  signal get : integer_vector(0 to 2);
  signal kawnlia : time;
  signal fbht : integer_vector(0 to 2);
begin
  yrda : entity work.ctvtdjdac
    port map (siigj => vprcbduhjj, meksfih => fbht);
  pbfekl : entity work.ctvtdjdac
    port map (siigj => kawnlia, meksfih => get);
  fgg : entity work.ctvtdjdac
    port map (siigj => r, meksfih => cr);
  bxu : entity work.ctvtdjdac
    port map (siigj => kmxrqsyw, meksfih => tghekizol);
  
  -- Single-driven assignments
  kawnlia <= 2.0 fs;
  kmxrqsyw <= kawnlia;
  r <= kawnlia;
  mvbhzouaf <= (8#0_6_3_3# ps, 3_3_3_2 fs);
  
  -- Multi-driven assignments
  q <= 'Z';
end dtzn;



-- Seed after: 16999743495544139978,12269339630485015285
