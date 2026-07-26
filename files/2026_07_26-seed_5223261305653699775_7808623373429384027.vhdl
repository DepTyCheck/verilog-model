-- Seed: 5223261305653699775,7808623373429384027

entity tjca is
  port (t : in integer; kwxiypa : inout time);
end tjca;

architecture weyntmyb of tjca is
  
begin
  
end weyntmyb;

entity txvfzonhrz is
  port (apevr : in real);
end txvfzonhrz;

architecture chdub of txvfzonhrz is
  signal vndtp : time;
  signal tonxy : time;
  signal vfgnapafrc : integer;
  signal iffde : time;
  signal lwhadzjc : integer;
begin
  mlsknfnclh : entity work.tjca
    port map (t => lwhadzjc, kwxiypa => iffde);
  bb : entity work.tjca
    port map (t => vfgnapafrc, kwxiypa => tonxy);
  qnr : entity work.tjca
    port map (t => lwhadzjc, kwxiypa => vndtp);
  
  -- Single-driven assignments
  vfgnapafrc <= 0;
  lwhadzjc <= lwhadzjc;
end chdub;

entity zebcpllq is
  port (y : inout integer; uxcz : out integer);
end zebcpllq;

architecture gsim of zebcpllq is
  signal spibtct : time;
  signal nnhtiuhsu : real;
begin
  eok : entity work.txvfzonhrz
    port map (apevr => nnhtiuhsu);
  ockbt : entity work.tjca
    port map (t => uxcz, kwxiypa => spibtct);
  guixoctwdy : entity work.txvfzonhrz
    port map (apevr => nnhtiuhsu);
  
  -- Single-driven assignments
  y <= uxcz;
  uxcz <= 8#7_6#;
end gsim;



-- Seed after: 12219902373281296860,7808623373429384027
