-- Seed: 18424992802263776193,10871023049702252113

entity xcandman is
  port (qzmyssnlsw : buffer real; qk : in integer_vector(3 to 3));
end xcandman;

architecture vzrguwuch of xcandman is
  
begin
  -- Single-driven assignments
  qzmyssnlsw <= 8#0.240#;
end vzrguwuch;

entity tbuk is
  port (nkklwmntz : inout real);
end tbuk;

architecture yymi of tbuk is
  signal wfyaqxh : integer_vector(3 to 3);
  signal yimqrv : real;
  signal f : integer_vector(3 to 3);
  signal clapygrjtj : real;
  signal tfsiaoueih : integer_vector(3 to 3);
begin
  ypxzixkxtk : entity work.xcandman
    port map (qzmyssnlsw => nkklwmntz, qk => tfsiaoueih);
  xpdncj : entity work.xcandman
    port map (qzmyssnlsw => clapygrjtj, qk => f);
  leimoqc : entity work.xcandman
    port map (qzmyssnlsw => yimqrv, qk => wfyaqxh);
end yymi;

entity gxufl is
  port (kujzrzrbj : buffer time);
end gxufl;

architecture srp of gxufl is
  signal fbppmfosk : integer_vector(3 to 3);
  signal uzjclm : real;
  signal ftts : integer_vector(3 to 3);
  signal kor : real;
  signal ijjvqvr : integer_vector(3 to 3);
  signal npyyunfho : real;
  signal oibtoxszpi : integer_vector(3 to 3);
  signal gylaskm : real;
begin
  nshiaomej : entity work.xcandman
    port map (qzmyssnlsw => gylaskm, qk => oibtoxszpi);
  xbayofl : entity work.xcandman
    port map (qzmyssnlsw => npyyunfho, qk => ijjvqvr);
  n : entity work.xcandman
    port map (qzmyssnlsw => kor, qk => ftts);
  edtfdpzmqi : entity work.xcandman
    port map (qzmyssnlsw => uzjclm, qk => fbppmfosk);
  
  -- Single-driven assignments
  ijjvqvr <= ftts;
end srp;



-- Seed after: 11069753595830319319,10871023049702252113
