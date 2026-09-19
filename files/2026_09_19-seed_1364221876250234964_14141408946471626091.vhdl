-- Seed: 1364221876250234964,14141408946471626091

entity clqbiwcc is
  port (paqnxbld : buffer real; tiyr : linkage integer; se : buffer integer);
end clqbiwcc;

architecture wffo of clqbiwcc is
  
begin
  -- Single-driven assignments
  se <= 8#1#;
  paqnxbld <= 16#2_0_4_1.2_B#;
end wffo;

entity evaqqyzbg is
  port (ce : out time; kli : buffer bit_vector(1 downto 0); ncnhqhhgt : buffer real);
end evaqqyzbg;

architecture dmaqncwdrw of evaqqyzbg is
  signal olq : integer;
  signal doqryweq : integer;
  signal oav : real;
  signal vodyj : integer;
  signal pvby : integer;
begin
  sgpzx : entity work.clqbiwcc
    port map (paqnxbld => ncnhqhhgt, tiyr => pvby, se => vodyj);
  ma : entity work.clqbiwcc
    port map (paqnxbld => oav, tiyr => doqryweq, se => olq);
  
  -- Single-driven assignments
  kli <= ('0', '0');
  ce <= 0.4_3_4_1 fs;
end dmaqncwdrw;

entity i is
  port (vnfnmtjx : in bit_vector(2 downto 3); sk : in integer; ybt : linkage real; blfubhqnh : out integer);
end i;

architecture rrtob of i is
  signal shlmsgnwg : integer;
  signal pdcpvg : real;
  signal lkfw : integer;
  signal oqwlfbp : integer;
  signal vciqgv : real;
  signal wojgk : real;
  signal ksrwa : bit_vector(1 downto 0);
  signal sjh : time;
  signal wkvdmy : integer;
  signal ibu : integer;
  signal cacwwar : real;
begin
  ncndxhsdpu : entity work.clqbiwcc
    port map (paqnxbld => cacwwar, tiyr => ibu, se => wkvdmy);
  jj : entity work.evaqqyzbg
    port map (ce => sjh, kli => ksrwa, ncnhqhhgt => wojgk);
  mfglpsq : entity work.clqbiwcc
    port map (paqnxbld => vciqgv, tiyr => oqwlfbp, se => lkfw);
  wkxibdt : entity work.clqbiwcc
    port map (paqnxbld => pdcpvg, tiyr => shlmsgnwg, se => blfubhqnh);
end rrtob;



-- Seed after: 14853926147002660589,14141408946471626091
