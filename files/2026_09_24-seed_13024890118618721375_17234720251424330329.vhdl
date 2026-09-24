-- Seed: 13024890118618721375,17234720251424330329

entity mke is
  port (cqokyo : in integer; wz : buffer integer; e : inout boolean_vector(4 downto 4));
end mke;

architecture guuisaljxo of mke is
  
begin
  -- Single-driven assignments
  wz <= 8#4_0_3#;
  e <= e;
end guuisaljxo;

entity emcoq is
  port (ui : out boolean);
end emcoq;

architecture l of emcoq is
  signal fubfx : boolean_vector(4 downto 4);
  signal stcibkl : integer;
  signal r : boolean_vector(4 downto 4);
  signal xbhtpfx : integer;
  signal spdhbwd : boolean_vector(4 downto 4);
  signal dzuy : integer;
  signal yxquoe : integer;
  signal yilbaxftg : boolean_vector(4 downto 4);
  signal pxeyqtft : integer;
  signal f : integer;
begin
  tixu : entity work.mke
    port map (cqokyo => f, wz => pxeyqtft, e => yilbaxftg);
  xitugluwx : entity work.mke
    port map (cqokyo => yxquoe, wz => dzuy, e => spdhbwd);
  sesviikxy : entity work.mke
    port map (cqokyo => xbhtpfx, wz => f, e => r);
  afui : entity work.mke
    port map (cqokyo => stcibkl, wz => xbhtpfx, e => fubfx);
  
  -- Single-driven assignments
  ui <= TRUE;
  stcibkl <= 2#1_0_0_1_0#;
  yxquoe <= 1;
end l;



-- Seed after: 13213726416595042044,17234720251424330329
