-- Seed: 5954978469767199131,6176025545472508329

entity ftiy is
  port (iewlmk : inout severity_level);
end ftiy;

architecture qf of ftiy is
  
begin
  -- Single-driven assignments
  iewlmk <= FAILURE;
end qf;

entity vqyag is
  port (jv : out severity_level; vw : out real_vector(1 to 1); limrrxi : buffer time_vector(0 downto 1));
end vqyag;

architecture vlmxj of vqyag is
  signal gtqyjmkjx : severity_level;
  signal lppw : severity_level;
  signal pfjoffjyu : severity_level;
begin
  ujxwwhyptx : entity work.ftiy
    port map (iewlmk => pfjoffjyu);
  ymwsazkyo : entity work.ftiy
    port map (iewlmk => jv);
  t : entity work.ftiy
    port map (iewlmk => lppw);
  zzyv : entity work.ftiy
    port map (iewlmk => gtqyjmkjx);
  
  -- Single-driven assignments
  vw <= (others => 16#7F.5_5_B#);
  limrrxi <= limrrxi;
end vlmxj;

entity uihrda is
  port (ietrvl : out boolean);
end uihrda;

architecture envwevmvp of uihrda is
  signal rbzosdnurz : severity_level;
begin
  zgjqqckio : entity work.ftiy
    port map (iewlmk => rbzosdnurz);
  
  -- Single-driven assignments
  ietrvl <= TRUE;
end envwevmvp;



-- Seed after: 8082778503468955012,6176025545472508329
