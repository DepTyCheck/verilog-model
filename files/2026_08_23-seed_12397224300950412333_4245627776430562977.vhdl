-- Seed: 12397224300950412333,4245627776430562977

entity ak is
  port (xw : in character; romfzvzgmn : in time; b : buffer integer; owhrfgl : linkage time);
end ak;

architecture isgny of ak is
  
begin
  -- Single-driven assignments
  b <= 3;
end isgny;

entity xrvpbw is
  port (emhd : linkage time; tghdha : linkage time);
end xrvpbw;

architecture ffomqosy of xrvpbw is
  signal zyumzefqhv : integer;
  signal ppowvufkjn : character;
  signal dctitpragm : integer;
  signal yqrnc : time;
  signal wdzkv : character;
begin
  ydlz : entity work.ak
    port map (xw => wdzkv, romfzvzgmn => yqrnc, b => dctitpragm, owhrfgl => tghdha);
  nx : entity work.ak
    port map (xw => ppowvufkjn, romfzvzgmn => yqrnc, b => zyumzefqhv, owhrfgl => emhd);
  
  -- Single-driven assignments
  ppowvufkjn <= wdzkv;
end ffomqosy;

entity huwrm is
  port (ljcjwv : in real; wkksyglast : linkage time; qfdhbzer : out real);
end huwrm;

architecture os of huwrm is
  signal xbcgtsfho : time;
  signal ibzcjzv : integer;
  signal rzze : time;
  signal iuplf : integer;
  signal dvacaynwci : time;
  signal qbini : character;
  signal riui : integer;
  signal smpxmiwq : character;
  signal lp : time;
  signal riluyldp : time;
begin
  s : entity work.xrvpbw
    port map (emhd => riluyldp, tghdha => lp);
  cek : entity work.ak
    port map (xw => smpxmiwq, romfzvzgmn => lp, b => riui, owhrfgl => wkksyglast);
  ux : entity work.ak
    port map (xw => qbini, romfzvzgmn => dvacaynwci, b => iuplf, owhrfgl => rzze);
  vamyi : entity work.ak
    port map (xw => smpxmiwq, romfzvzgmn => riluyldp, b => ibzcjzv, owhrfgl => xbcgtsfho);
  
  -- Single-driven assignments
  qfdhbzer <= 16#2_5_2.9_1_5_5#;
  smpxmiwq <= 'g';
end os;



-- Seed after: 1330638317533723740,4245627776430562977
