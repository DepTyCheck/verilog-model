-- Seed: 2226036384761664334,10240345754018108067



entity dowq is
  port (jyy : inout integer; oahyukgep : buffer boolean_vector(3 downto 3); czb : inout time_vector(2 downto 2); b : in boolean_vector(2 to 4));
end dowq;



architecture fajqyjs of dowq is
  
begin
  
end fajqyjs;



entity wwmivab is
  port (waoe : out real);
end wwmivab;



architecture t of wwmivab is
  signal yvbzrmvfm : boolean_vector(2 to 4);
  signal bctwds : time_vector(2 downto 2);
  signal vxt : boolean_vector(3 downto 3);
  signal paflfaayr : integer;
  signal anscmfey : boolean_vector(2 to 4);
  signal nkmvzhyx : time_vector(2 downto 2);
  signal ldkyl : boolean_vector(3 downto 3);
  signal ayfd : integer;
  signal mqazoi : boolean_vector(2 to 4);
  signal hvjuimzv : time_vector(2 downto 2);
  signal gf : boolean_vector(3 downto 3);
  signal b : integer;
begin
  hdixoxkva : entity work.dowq
    port map (jyy => b, oahyukgep => gf, czb => hvjuimzv, b => mqazoi);
  jjatwkzgix : entity work.dowq
    port map (jyy => ayfd, oahyukgep => ldkyl, czb => nkmvzhyx, b => anscmfey);
  wvmybwrnl : entity work.dowq
    port map (jyy => paflfaayr, oahyukgep => vxt, czb => bctwds, b => yvbzrmvfm);
end t;

library ieee;
use ieee.std_logic_1164.all;

entity xe is
  port (kjm : in std_logic_vector(4 to 1); mysieaw : out bit; qkdlptygnx : in integer; jy : inout time);
end xe;



architecture copjlcnajr of xe is
  signal idqxtdz : time_vector(2 downto 2);
  signal dowhpyoi : boolean_vector(3 downto 3);
  signal ztfsm : integer;
  signal zumbfanw : time_vector(2 downto 2);
  signal mjy : boolean_vector(3 downto 3);
  signal oim : integer;
  signal lfgulfmr : boolean_vector(2 to 4);
  signal uil : time_vector(2 downto 2);
  signal ex : boolean_vector(3 downto 3);
  signal nz : integer;
begin
  rkingoenp : entity work.dowq
    port map (jyy => nz, oahyukgep => ex, czb => uil, b => lfgulfmr);
  kmlg : entity work.dowq
    port map (jyy => oim, oahyukgep => mjy, czb => zumbfanw, b => lfgulfmr);
  t : entity work.dowq
    port map (jyy => ztfsm, oahyukgep => dowhpyoi, czb => idqxtdz, b => lfgulfmr);
end copjlcnajr;



-- Seed after: 4596044459719649213,10240345754018108067
